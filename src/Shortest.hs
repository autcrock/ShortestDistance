{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    shortest
) where

    import Data.Aeson
    import Data.List (sortBy, nub, find, delete, deleteBy)
    import Data.Maybe (fromJust, fromMaybe, isNothing)
    import Data.Ord (comparing)
    import Data.Text (Text, pack)
    import GHC.Generics hiding (from, to)
    import MapDefinitions (
        Map, Place, Destination, place, destinations, distance, map, readMap, to
        )
    import Numeric.Natural (Natural)

    data Connection = Connection { from :: Text, to :: Text, distance :: Double } deriving (Show)
    
    data Child = Child { childName :: Text, childDistance :: Double} deriving (Show, Generic)
    instance ToJSON Child
    instance FromJSON Child

    data Vertex = Vertex {
            vertexName :: Text, value :: Double, neighbours :: [Child]
        } deriving (Show, Generic)
    instance ToJSON Vertex
    instance FromJSON Vertex

    data Graph = Graph {vertices :: [Vertex]} deriving (Show, Generic)
    instance ToJSON Graph
    instance FromJSON Graph

    sortByDistance :: [Child] -> [Child]
    sortByDistance = sortBy (comparing childDistance)

    getVertexNames :: [Connection] -> [Text]
    getVertexNames = nub . Prelude.map from

    pullVertex :: Text -> [Connection] -> Double -> Vertex
    pullVertex vertexName cs value =
        let
            rawVertices = filter (\x -> from x == vertexName) cs
            neighbours = Prelude.map (\x -> Child {
                 childName = Shortest.to x
                 , Shortest.childDistance = Shortest.distance x
                 }) rawVertices
        in
            Vertex {
                vertexName = vertexName
                , value = value
                , neighbours = sortByDistance neighbours
                }

    makeInfinity :: Double
    makeInfinity = read "Infinity" ::Double

    allVertices :: [Connection] -> Graph
    allVertices cs =
        let
            infinity = makeInfinity
            vertexNames = getVertexNames cs
            vertices = [ pullVertex x cs infinity | x <- vertexNames]
        in
            Graph {vertices = vertices}
    
    insertPlaceWD :: Place -> [Connection] -> [Connection]
    insertPlaceWD place connections =
        connections ++ expandPlace place
        
    expandPlace :: Place -> [Connection]
    expandPlace p =
        expandPlace' (place p) (destinations p) []

    expandPlace' :: Text -> [Destination] -> [Connection] -> [Connection]
    expandPlace' placeName [] connections = connections
    expandPlace' placeName [destination] connections =
         connections
         ++ [Connection {
             Shortest.from = placeName,
             Shortest.to = MapDefinitions.to destination,
             Shortest.distance = MapDefinitions.distance destination }] 
         ++ [Connection {
            Shortest.from = MapDefinitions.to destination,
            Shortest.to = placeName,
            Shortest.distance = MapDefinitions.distance destination }]
    expandPlace' placeName (d : destinations) connections =
        connections
        ++ [Connection {
            Shortest.from = placeName,
            Shortest.to = MapDefinitions.to d,
            Shortest.distance = MapDefinitions.distance d }] 
        ++ [Connection {
            Shortest.from = MapDefinitions.to d,
            Shortest.to = placeName,
            Shortest.distance = MapDefinitions.distance d }]
        ++ expandPlace' placeName destinations connections

    mapToWorkingData :: Map -> [Connection]
    mapToWorkingData map =
        let
            places = MapDefinitions.map map
        in
            if null places
                then []
                else mapToWorkingData' places []
    
    mapToWorkingData' :: [Place] -> [Connection] -> [Connection]
    mapToWorkingData' [] done = done
    mapToWorkingData' [place] done  = insertPlaceWD place done
    mapToWorkingData' (place : places) done =
        mapToWorkingData' [place] done ++ mapToWorkingData' places done

    mapToGraph :: Map -> Graph
    mapToGraph m =
        let
            wd = mapToWorkingData m
        in
            allVertices wd

    graphGetVertexChildren :: Graph -> Text -> Maybe [Child]
    graphGetVertexChildren pg vertex = 
        let
            n = graphGetVertex pg vertex
        in
            if isNothing n
            then
                Nothing
            else
                Just (neighbours $ fromJust n)
    
    graphGetClosestToVertex :: Graph -> Text -> Maybe Child
    graphGetClosestToVertex pg vertex =
        let
            n = graphGetVertex pg vertex
        in
            if isNothing n
            then
                Nothing
            else
                Just (head (neighbours $ fromJust n))
    
    graphGetVertex :: Graph -> Text -> Maybe Vertex
    graphGetVertex pg vertex = find (\x -> vertexName x == vertex) (vertices pg)
    
    graphGetParameter :: Graph -> Text -> Maybe Double
    graphGetParameter pg vertex =
        let
            n = graphGetVertex pg vertex
        in
            if isNothing n
            then
                Nothing
            else
                Just (value $ fromJust n)
    
    graphDeleteVertex :: Graph -> Vertex -> Graph
    graphDeleteVertex pg vertex =
        let 
            vs = deleteBy (\x y -> vertexName x == vertexName vertex) vertex (vertices pg)
        in 
            Graph { vertices = vs }
            
    graphInsert  :: Graph -> Vertex -> Graph
    graphInsert pg vertex =
        Graph { vertices = vertex : vertices pg}

    transferVertexUpdatingParameter :: Graph -> Text -> Double -> Graph -> (Graph, Graph)
    transferVertexUpdatingParameter graph1 vName value_in graph2 =
        let
            txVertex = graphGetVertex graph1 vName
        in
            if isNothing txVertex
            then (graph1, graph2)
            else
                let 
                    txV = fromJust txVertex

                    parVertex = Vertex {
                        vertexName = vertexName txV
                        , value = value_in
                        , neighbours = neighbours txV
                    }
                    newGraph2 = graphInsert graph2 parVertex
                    newGraph1 = graphDeleteVertex graph1 txV
                in
                    (newGraph1, newGraph2) 

    transferVertex :: Graph -> Text -> Graph -> (Graph, Graph)
    transferVertex graph1 vName graph2 =
        let
            txVertex = graphGetVertex graph1 vName
        in
            if isNothing txVertex
            then (graph1, graph2)
            else
                let 
                    txN = fromJust txVertex
                    newGraph2 = graphInsert graph2 txN
                    newGraph1 = graphDeleteVertex graph1 txN
                in
                    (newGraph1, newGraph2) 

    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            let pg = mapToGraph m
            let (reds, yellows) =
                    transferVertexUpdatingParameter pg (pack from) 0.0 Graph{vertices = []}
            let greens = Graph{vertices = []}
            shortest' reds yellows greens (pack from) (pack to)

    shortest' :: Graph -> Graph -> Graph -> Text -> Text -> IO()
    shortest' reds yellows greens from to = 
        let
            v = head (vertices yellows)
            vs = neighbours v
        in
            if null vs
            then
                error "shortest': Unexpectedly could not find neighbours."
            else
                let
                    -- content = fromMaybe null neighbours
                    -- closest = head content
                    -- remainder = tail content
--                    closest = head ns
                    closest = graphGetClosestToVertex yellows from
                    storedDistance = childDistance $ fromJust closest 
--                    when isNothing closest $ error ("shortest': Unexpectedly found no closest vertex")
                    closestVertexName = childName $ fromJust closest
                    (ys1, gs1) = transferVertexUpdatingParameter yellows from storedDistance greens
                    (rs2, ys2) = transferVertexUpdatingParameter reds closestVertexName 0 yellows
                in
                    do
                        print "==================="
                        print ("reds = " ++ show reds)
                        print "==================="
                        print ("yellows = " ++ show yellows)
                        print "==================="
                        print ("greens = " ++ show greens)
                        print "==================="
                        print ("closest vertex = " ++ show closestVertexName)
                        print "==================="
                        print ("ys1 = " ++ show ys1)
                        print "==================="
                        print ("gs1 = " ++ show gs1)
                        print "==================="
                        print ("rs2 = " ++ show rs2)
                        print "==================="
                        print ("ys2 = " ++ show ys2)
                        print "==================="
