{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    shortest
) where

    import Data.Aeson
    import Data.List (sortBy, nub, find, delete, deleteBy)
    import Data.Maybe (fromJust, fromMaybe, mapMaybe, isNothing)
    import Data.Ord (comparing)
    import Data.Text (Text, pack)
    import Debug.Trace
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

    data AccumulateOrReplace = Accumulate | Replace deriving (Eq, Show)

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
        deleteBy (\x y -> vertex x == vertex y) v vs

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
    
    deleteNeighbour :: [Neighbour] -> Text -> [Neighbour]
    deleteNeighbour ns name =
        deleteBy (\x y -> neighbour x == y) name ns

    accumulateOrReplace :: Double -> Double -> AccumulateOrReplace -> Double
    accumulateOrReplace oldValue valueForConsideration accumulate =
        if accumulate == Accumulate
        then
            let 
                newValue = oldValue + valueForConsideration 
            in
                if oldValue > newValue
                then
                    newValue
                else
                    oldValue
        else
           valueForConsideration

    transferVerticesUpdatingAccumulatedDistance :: (Graph, Graph) -> [Text] -> Text -> Double -> AccumulateOrReplace -> (Graph, Graph)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) [] notThisOne distance_in accumulate =
        (graph1, graph2)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) [vName] notThisOne distance_in accumulate =
            transferVertexUpdatingAccumulatedDistance (graph1, graph2) vName notThisOne distance_in accumulate
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) (vName:vNames) notThisOne distance_in accumulate =
        let 
            (graph1', graph2') = 
                transferVertexUpdatingAccumulatedDistance (graph1, graph2) vName notThisOne distance_in accumulate
        in
                transferVerticesUpdatingAccumulatedDistance (graph1', graph2') vNames notThisOne distance_in accumulate

    transferVertexUpdatingAccumulatedDistance :: (Graph, Graph) -> Text -> Text -> Double -> AccumulateOrReplace -> (Graph, Graph)
    transferVertexUpdatingAccumulatedDistance (graph1, graph2) vName notThisOne distance_in accumulate =
        let
            txVertex = graphGetVertex graph1 vName
        in
            if isNothing txVertex
            then (graph1, graph2)
            else
                let 
                    txV = fromJust txVertex

                    v = Vertex {
                        vertex = vertex txV
                        , accumulatedDistance = accumulateOrReplace (accumulatedDistance txV) distance_in accumulate
                        , neighbours = if notThisOne == "" 
                                       then
                                           neighbours txV
                                       else
                                         deleteNeighbour notThisOne $ neighbours txV
                    }
                    
                    newGraph2 = graphInsertVertex graph2 v
                    newGraph1 = graphDeleteVertex graph1 txV
                in
                    (newGraph1, newGraph2) 

    tellTheNeighbours :: Text -> Text -> (Graph, Graph) -> [Neighbour] -> Double -> (Graph, Graph)
    tellTheNeighbours vertexName previousName (reds, yellows) neighbours distance_in =
        let
            neighbourNames = Prelude.map neighbour neighbours
            redNeighbours = Prelude.map vertex $ mapMaybe (graphGetVertex reds) neighbourNames
            yellowNeighbours = Prelude.map vertex $ mapMaybe (graphGetVertex yellows) neighbourNames
            (rs', ys') = transferVerticesUpdatingAccumulatedDistance (reds, yellows) redNeighbours previousName distance_in Replace
            (_, ys'') = transferVerticesUpdatingAccumulatedDistance (ys', ys') yellowNeighbours previousName distance_in Accumulate
        in 
            (rs', ys'')

    vertexNames :: [Vertex] -> [Text]
    vertexNames = Prelude.map vertex

    graphVertexNames :: Graph -> [Text]
    graphVertexNames g = vertexNames (vertices g)
                    
    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            let pg = mapToGraph m
            let (reds, yellows) =
                    transferVertexUpdatingAccumulatedDistance (pg, Graph{vertices = []}) (pack from) "" 0.0 Replace
            let greens = Graph{vertices = []}
            let distance = 
                  trace ( "shortest': rs: " ++ show (graphVertexNames reds)
                    ++ " ys: " ++ show (graphVertexNames yellows)
                    ++ " gs: " ++ show (graphVertexNames greens)
                    ++ " from: " ++ from ++ " to: " ++ to ++ " closest: " ++ from
                    ++ " nextDistance: " ++ show 0 )
                    shortest' reds yellows greens (pack from) (pack to) (pack from) 0
            print ("Shortest distance from [" ++ from ++ "] to [" ++ to ++ "] = " ++ show distance)

    shortest' :: Graph -> Graph -> Graph -> Text -> Text -> Text -> Double -> Maybe Double
    shortest' reds yellows greens fromName toName currentVertexName currentDistance = 
        if currentVertexName == toName
        then
            Just currentDistance
        else
            do
                closest <- graphGetClosestToVertex yellows currentVertexName
                let closestDistance = howFar closest
                    closestVertexName = neighbour closest
                    nextDistance = closestDistance + currentDistance
                    (ys1, gs1) = transferVertexUpdatingAccumulatedDistance (yellows, greens) currentVertexName "" nextDistance Replace
                    neighbours = graphGetVertexNeighbours gs1 currentVertexName
                    (rs2, ys2) = tellTheNeighbours currentVertexName (reds, ys1) (fromJust neighbours) currentDistance
                trace ( "shortest': rs: " ++ show (graphVertexNames rs2)
                         ++ " ys: " ++ show (graphVertexNames ys2)
                         ++ " gs: " ++ show (graphVertexNames gs1)
                         ++ " from: " ++ show fromName ++ " to: " ++ show toName ++ " closest: " ++ show closestVertexName
                         ++ " nextDistance: " ++ show nextDistance )
                        shortest' rs2 ys2 gs1 fromName toName closestVertexName nextDistance


            -- in 
            --     do
            --         print "==================="
            --         print ("reds = " ++ show reds)
            --         print "==================="
            --         print ("yellows = " ++ show yellows)
            --         print "==================="
            --         print ("greens = " ++ show greens)
            --         print "==================="
            --         print ("closest vertex = " ++ show closestVertexName)
            --         print "==================="
            --         print ("ys1 = " ++ show ys1)
            --         print "==================="
            --         print ("gs1 = " ++ show gs1)
            --         print "==================="
            --         print ("fromName = " ++ show fromName ++ "; closestDistance = " ++ show closestDistance)
            --         print "==================="
            --         print ("neighbours = " ++ show neighbours)
            --         print "==================="
            --         print ("rs2 = " ++ show rs2)
            --         print "==================="
            --         print ("ys2 = " ++ show ys2)
            --         print "==================="
