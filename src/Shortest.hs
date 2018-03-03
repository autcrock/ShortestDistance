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
    
    data Neighbour = Neighbour { neighbour :: Text, howFar :: Double} deriving (Show, Generic)
    instance ToJSON Neighbour
    instance FromJSON Neighbour

    data Vertex = Vertex {
            vertex :: Text, accumulatedDistance :: Double, neighbours :: [Neighbour]
        } deriving (Show, Generic)
    instance ToJSON Vertex
    instance FromJSON Vertex

    data Graph = Graph {vertices :: [Vertex]} deriving (Show, Generic)
    instance ToJSON Graph
    instance FromJSON Graph

    sortByDistance :: [Neighbour] -> [Neighbour]
    sortByDistance = sortBy (comparing howFar)

    getVertexNames :: [Connection] -> [Text]
    getVertexNames = nub . Prelude.map from

    pullVertex :: Text -> [Connection] -> Double -> Vertex
    pullVertex vertex cs accumulatedDistance =
        let
            rawVertices = filter (\x -> from x == vertex) cs
            neighbours = Prelude.map (\x -> Neighbour {
                 neighbour = Shortest.to x
                 , howFar = Shortest.distance x
                 }) rawVertices
        in
            Vertex {
                vertex = vertex
                , accumulatedDistance = accumulatedDistance
                , neighbours = sortByDistance neighbours
                }

    makeInfinity :: Double
    makeInfinity = read "Infinity" ::Double

    connectionsToGraph :: [Connection] -> Graph
    connectionsToGraph cs =
        let
            infinity = makeInfinity
            vertices = getVertexNames cs
        in
            Graph {vertices = [ pullVertex x cs infinity | x <- vertices ]}
    
    insertPlaceInConnections :: Place -> [Connection] -> [Connection]
    insertPlaceInConnections place connections =
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

    mapToConnections :: Map -> [Connection]
    mapToConnections map =
        let
            places = MapDefinitions.map map
        in
            if null places
                then []
                else mapToConnections' places []
    
    mapToConnections' :: [Place] -> [Connection] -> [Connection]
    mapToConnections' [] done = done
    mapToConnections' [place] done  = insertPlaceInConnections place done
    mapToConnections' (place : places) done =
        mapToConnections' [place] done ++ mapToConnections' places done

    mapToGraph :: Map -> Graph
    mapToGraph m =
        let
            connections = mapToConnections m
        in
            connectionsToGraph connections

    graphGetVertexNeighbours :: Graph -> Text -> Maybe [Neighbour]
    graphGetVertexNeighbours pg vertex = 
        fmap neighbours (graphGetVertex pg vertex)
        -- graphGetVertex pg vertex >>= return . neighbours

        -- do
        --     v <- graphGetVertex pg vertex
        --     return $ neighbours v
    
    graphGetClosestToVertex :: Graph -> Text -> Maybe Neighbour
    graphGetClosestToVertex pg vertex =
        fmap head (graphGetVertexNeighbours pg vertex)

    getVertex :: [Vertex] -> Text -> Maybe Vertex
    getVertex vs vName =
        find (\x -> vertex x == vName) vs

    graphGetVertex :: Graph -> Text -> Maybe Vertex
    graphGetVertex pg = getVertex (vertices pg)
    
    graphGetAccumulatedDistance :: Graph -> Text -> Maybe Double
    graphGetAccumulatedDistance pg vertex =
        fmap accumulatedDistance (graphGetVertex pg vertex)

    deleteVertex :: [Vertex] -> Vertex -> [Vertex]
    deleteVertex vs v =
        deleteBy (\x _ -> vertex x == vertex v) v vs

    graphDeleteVertex :: Graph -> Vertex -> Graph
    graphDeleteVertex pg v =
        let 
            vs = deleteVertex (vertices pg) v
        in 
            Graph { vertices = vs }
    
    insertVertex :: [Vertex] -> Vertex -> [Vertex]
    insertVertex vs v = v:vs

    graphInsertVertex  :: Graph -> Vertex -> Graph
    graphInsertVertex pg vertex =
        Graph { vertices = insertVertex (vertices pg) vertex }

    transferVerticesUpdatingAccumulatedDistance :: (Graph, Graph) -> [Text] -> Double -> (Graph, Graph)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) [] accumulatedDistance_in = (graph1, graph2)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) (vName:vNames) accumulatedDistance_in =
        let 
            (graph1', graph2') = transferVertexUpdatingAccumulatedDistance (graph1, graph2) vName accumulatedDistance_in
        in
            transferVerticesUpdatingAccumulatedDistance (graph1', graph2') vNames accumulatedDistance_in

    transferVertexUpdatingAccumulatedDistance :: (Graph, Graph) -> Text -> Double -> (Graph, Graph)
    transferVertexUpdatingAccumulatedDistance (graph1, graph2) vName accumulatedDistance_in =
        let
            txVertex = graphGetVertex graph1 vName
        in
            if isNothing txVertex
            then (graph1, graph2)
            else
                let 
                    txV = fromJust txVertex

                    parVertex = Vertex {
                        vertex = vertex txV
                        , accumulatedDistance = accumulatedDistance_in
                        , neighbours = neighbours txV
                    }
                    newGraph2 = graphInsertVertex graph2 parVertex
                    newGraph1 = graphDeleteVertex graph1 txV
                in
                    (newGraph1, newGraph2) 

    -- transferVertex :: (Graph, Graph) -> Text -> (Graph, Graph)
    -- transferVertex (graph1, graph2) vName =
    --     let
    --         txVertex = graphGetVertex graph1 vName
    --     in
    --         if isNothing txVertex
    --         then (graph1, graph2)
    --         else
    --             let 
    --                 txN = fromJust txVertex
    --                 newGraph2 = graphInsertVertex graph2 txN
    --                 newGraph1 = graphDeleteVertex graph1 txN
    --             in
    --                 (newGraph1, newGraph2) 

    tellTheNeighbours :: Text -> (Graph, Graph) -> Double -> Maybe (Graph, Graph)
    tellTheNeighbours vt (reds, yellows) accumulatedDistance_in =
        let
            vs = vertices reds
            ns = graphGetVertexNeighbours reds vt
        in 
            if isNothing ns
            then Nothing
            else
                do 
                    let names = Prelude.map neighbour $ fromJust ns 
                    let (reds1, yellows1) = transferVerticesUpdatingAccumulatedDistance (reds, yellows) names accumulatedDistance_in
                    Just (reds1, yellows1)

    -- tellTheNeighbours' :: Double -> [Vertex] -> [Neighbour] -> Maybe [Vertex]
    -- tellTheNeighbours' _ [] _ = Just []
    -- tellTheNeighbours' _ vs [] = Just vs
    -- tellTheNeighbours' accumulatedDistance_in vs (n:ns) =
    --     let
    --         nName = neighbour n
    --         v = getVertex vs nName
    --     in 
    --         if isNothing v
    --         then Nothing
    --         else
    --             let
    --                 v1 = fromJust v
    --                 vs1 = deleteVertex vs v1
    --                 vs2 = insertVertex vs Vertex {
    --                     vertex = nName
    --                     , accumulatedDistance = accumulatedDistance_in
    --                     , neighbours = neighbours v1
    --                     }
    --             in
    --                 tellTheNeighbours' accumulatedDistance_in vs2 ns
    
    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            let pg = mapToGraph m
            let (reds, yellows) =
                    transferVertexUpdatingAccumulatedDistance (pg, Graph{vertices = []}) (pack from) 0.0
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
                    storedDistance = howFar $ fromJust closest 
--                    when isNothing closest $ error ("shortest': Unexpectedly found no closest vertex")
                    closestVertexName = neighbour $ fromJust closest
                    (ys1, gs1) = transferVertexUpdatingAccumulatedDistance (yellows, greens) from storedDistance
                    -- (rs3, ys3) = transferVertexUpdatingAccumulatedDistance (reds, yellows) closestVertexName 0
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
                        print ("(rs2, ys2) = " ++ (show $ tellTheNeighbours from (reds, ys1) storedDistance))
                        print "==================="
                        -- print ("rs3 = " ++ show rs3)
                        -- print "==================="
                        -- print ("ys3 = " ++ show ys3)
                        -- print "==================="
