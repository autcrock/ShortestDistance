{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    shortest
) where

    import Data.Aeson
    import Data.List (sortBy, nub, find, delete, deleteBy)
    import Data.Maybe (fromJust, fromMaybe, mapMaybe, isNothing)
    import Data.Ord (comparing, min)
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

    data OptionalCompare = Compare | NoCompare deriving (Eq, Show)

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
    graphGetVertexNeighbours g vertex = 
        fmap neighbours (graphGetVertex g vertex)
        -- graphGetVertex g vertex >>= return . neighbours

        -- do
        --     v <- graphGetVertex g vertex
        --     return $ neighbours v
    
    deleteNeighbour :: [Neighbour] -> Neighbour -> [Neighbour]
    deleteNeighbour ns n =
        deleteBy (\x y -> neighbour x == neighbour y) n ns

    deleteNeighbourByName :: [Neighbour] -> Text -> [Neighbour]
    deleteNeighbourByName ns name =
        deleteNeighbour ns Neighbour {neighbour = name, howFar = 0}

    deleteNeighboursByName :: [Neighbour] -> [Text] -> [Neighbour]
    deleteNeighboursByName [] _ = []
    deleteNeighboursByName ns [] = ns
    deleteNeighboursByName ns (name:names) =
        deleteNeighboursByName (deleteNeighbourByName ns name) names

    neighbourHowFarByName :: [Neighbour] -> Text -> Double
    neighbourHowFarByName ns name =
        if null ns
        then
            0
        else
            let
                n = 
                    trace ("getNeighbour called by neighbourHowFarByName: ns: " ++ show ns ++ ", name: " ++ show name) $
                    getNeighbour ns name
            in
                maybe (error "neighbourHowFarByName: Error: Unexpected Nothing returned by getNeighbour.") howFar n

    graphGetAdmissibleVertexNeighbours :: Graph -> Text -> Graph -> Maybe [Neighbour]
    graphGetAdmissibleVertexNeighbours g currentVertexName greens =
        let
            gvs = vertices greens
        in
            if null gvs
            then 
                graphGetVertexNeighbours g currentVertexName
            else
                do 
                    let greenNames = Prelude.map vertex gvs
                    ns <- graphGetVertexNeighbours g currentVertexName
                    if null ns
                    then Nothing
                    else return $ deleteNeighboursByName ns greenNames

    graphGetClosestToVertex :: Graph -> Text -> Graph -> Maybe Neighbour
    graphGetClosestToVertex g vertex greens =
        fmap head (graphGetAdmissibleVertexNeighbours g vertex greens)

    getVertex :: [Vertex] -> Text -> Maybe Vertex
    getVertex vs vName =
        find (\x -> vertex x == vName) vs

    getNeighbour :: [Neighbour] -> Text -> Maybe Neighbour
    getNeighbour ns nName =
        -- trace ("getNeighbour calling find on: ns: " ++ show ns ++ ", nName" ++ show nName) $
        find (\x -> neighbour x == nName) ns
        
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
    
    compareOrNot :: Double -> Double -> OptionalCompare -> Double
    compareOrNot oldValue valueForConsideration compare =
            oldValue + valueForConsideration 

    transferVerticesUpdatingAccumulatedDistance :: (Graph, Graph) -> [Neighbour] -> [Vertex] -> Double -> OptionalCompare -> (Graph, Graph)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) _ [] _ _ =
        (graph1, graph2)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) neighbours [vertex] currentDistance compare =
        -- trace ("transferVerticesUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1: " ++ show (graphVertexNames graph1) ++ ", graph2: " ++ show (graphVertexNames graph2)
        -- ++ ", neighbours:" ++ show neighbours ++ ", vName: " ++ show vName ++ ", currentDistance: " ++ show currentDistance) $
           transferVertexUpdatingAccumulatedDistance (graph1, graph2) neighbours vertex currentDistance compare
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) neighbours (vertex:vertices) currentDistance compare =
        let 
            (graph1', graph2') =
                -- trace ("transferVertexUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1: " ++ show (graphVertexNames graph1) ++ ", graph2: " ++ show (graphVertexNames graph2)
                --  ++ ", neighbours:" ++ show neighbours ++ ", vName: " ++ show vName ++ ", currentDistance: " ++ show currentDistance) $
                transferVertexUpdatingAccumulatedDistance (graph1, graph2) neighbours vertex currentDistance compare
        in
            -- trace ("transferVerticesUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1': " ++ show (graphVertexNames graph1') ++ ", graph2': " ++ show (graphVertexNames graph2')
            -- ++ ", neighbours: " ++ show neighbours ++ ", vNames: " ++ show vNames ++ ", currentDistance: " ++ show currentDistance) $
            transferVerticesUpdatingAccumulatedDistance (graph1', graph2') neighbours vertices currentDistance compare

    -- when accumulating we need the minimum current distance for this neighbour
    pickMinimumAccumulatedDistance :: Double -> Double -> Double -> OptionalCompare -> Double
    pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance compare =
        if compare == Compare
            then
                min accumulatedD (currentDistance + neighbourDistance)
            else
                currentDistance

    transferVertexUpdatingAccumulatedDistance :: (Graph, Graph) -> [Neighbour] -> Vertex -> Double -> OptionalCompare -> (Graph, Graph)
    transferVertexUpdatingAccumulatedDistance (graph1, graph2) neighbours_in txV currentDistance compare =
            -- if isNothing txV
            -- then (graph1, graph2)
            -- else
                let
                    currentVName = vertex txV
                    accumulatedD = accumulatedDistance txV
                    neighbourDistance =
                        trace ("neighbourHowFarByName called by transferVertexUpdatingAccumulatedDistance: neighbours_in: " ++ show neighbours_in
                         ++ ", currentVName: " ++ show currentVName)$ 
                        neighbourHowFarByName neighbours_in currentVName
                    
                    accumulatedD' = 
                        trace ("pickMinimumAccumulatedDistance called by transferVertexUpdatingAccumulatedDistance: accumulatedD: " ++ show accumulatedD
                        ++ ", neighbourDistance: " ++ show neighbourDistance ++ ", currentDistance: " ++ show currentDistance ++ ", compare: " ++ show compare) $
                        pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance compare

                    v =  Vertex {
                        vertex = vertex txV
                        , accumulatedDistance = accumulatedD'
                        , neighbours = neighbours txV
                    }
                    
                    graph2' = graphDeleteVertex graph2 txV
                    newGraph1 = graphDeleteVertex graph1 txV
                    newGraph2 = 
                       trace ("graphInsertVertex called by transferVertexUpdatingAccumulatedDistance: graph2': " ++ show (graphVertexNames graph2')
                             ++ ", v: " ++ show v
                             ++ ", txV: " ++ show txV
                           ) $
                              graphInsertVertex graph2' v
                in
                    (newGraph1, newGraph2) 

    transferVertex :: (Graph, Graph) -> Vertex -> (Graph, Graph)
    transferVertex (graph1, graph2) txVertex =
        -- if isNothing txVertex
        -- then (graph1, graph2)
        -- else
            let 
                txV = txVertex
                newGraph1 = graphDeleteVertex graph1 txV
                newGraph2 = 
                    graphInsertVertex graph2 txV
            in
                (newGraph1, newGraph2) 

    tellTheNeighbours :: (Graph, Graph) -> [Neighbour] -> Double -> (Graph, Graph)
    tellTheNeighbours (reds, yellows) ns currentDistance =
        let
            nNames = Prelude.map neighbour ns

            redVs = mapMaybe (graphGetVertex reds) nNames
            (reds', yellows') = 
                trace ("transferVerticesUpdatingAccumulatedDistance called by TTN: reds: " ++ show (graphVertexNames reds) ++ ", yellows: " ++ show (graphVertexNames yellows)
                    ++ ", neighbours: " ++ show (neighbourNames ns)
                    ++ ", redVs: " ++ show redVs ++ ", currentDistance: " ++ show currentDistance ++ ", compare: " ++ show Compare
                    ++ ", reds full: " ++ show reds ++ ", yellows full: " ++ show yellows)
                transferVerticesUpdatingAccumulatedDistance (reds, yellows) ns redVs currentDistance Compare

            yellowVs = mapMaybe (graphGetVertex yellows') nNames
            (_, yellows'') = 
                trace ("transferVerticesUpdatingAccumulatedDistance called by TTN: yellows' x 2: " ++ show (graphVertexNames yellows')
                ++ ", neighbours: " ++ show (neighbourNames ns)
                ++ ", yellowVs: " ++ show yellowVs ++ ", currentDistance: " ++ show currentDistance ++ ", compare: " ++ show Compare
                ++ ", yellows full: " ++ show yellows')
                transferVerticesUpdatingAccumulatedDistance (yellows', yellows') ns yellowVs currentDistance Compare
        in 
            (reds', yellows'')

    vertexNames :: [Vertex] -> [Text]
    vertexNames = Prelude.map vertex

    graphVertexNames :: Graph -> [Text]
    graphVertexNames g = vertexNames (vertices g)

    neighbourNames :: [Neighbour] -> [Text]
    neighbourNames = Prelude.map neighbour
                    
    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            let pg = mapToGraph m
            let distance =
                    do
                        currentVertex <- graphGetVertex pg (pack from)
                        let (reds, yellows) =
                                trace ("transferVertex called by shortest: pg: " ++ show (graphVertexNames pg) ++ ", graph2: " ++ show (graphVertexNames Graph{vertices = []})
                                ++ ", neighbours:" ++ show ([]::[Neighbour]) ++ ", vName: " ++ show from ++ ", nextDistance: " ++ show 0) $
                                        transferVertex (pg, Graph{vertices = []}) currentVertex
                        let greens = Graph{vertices = []}
                        trace ( "shortest' called by shortest: rs: " ++ show (graphVertexNames reds)
                                ++ ", ys: " ++ show (graphVertexNames yellows)
                                ++ ", gs: " ++ show (graphVertexNames greens)
                                ++ ", from: " ++ from ++ ", to: " ++ to ++ ", closest: " ++ from
                                ++ ", currentDistance: " ++ show 0  ++ ", ys full: " ++ show yellows ++ ", gs full: " ++ show greens ) $
                         shortest' reds yellows greens (pack from) (pack to) 0 currentVertex
            print ("Shortest distance from [" ++ from ++ "] to [" ++ to ++ "] = " ++ show distance)

    shortest' :: Graph -> Graph -> Graph -> Text -> Text -> Double -> Vertex -> Maybe Double
    shortest' reds yellows greens fromName toName currentDistance currentVertex = 
        let
            currentVertexName = vertex currentVertex
        in
        if vertex currentVertex == toName
        then
            let
                v = graphGetVertex yellows toName
                distance = min currentDistance (accumulatedDistance $ fromJust v)
            in
                return distance
        else
            do
                neighbours <- graphGetAdmissibleVertexNeighbours yellows currentVertexName greens
                if null neighbours
                then error "shortest': unexpectedly no neighbours returned by graphGetAdmissibleVertexNeighbours."
                else
                    do 
                    let (reds', yellows', greens') = 
                            shortest'' reds yellows greens fromName toName currentDistance neighbours
                    closest <- 
                        trace ("graphGetClosestToVertex called by shortest': yellows = " ++ show (graphVertexNames yellows)
                            ++ ", currentVertexName = " ++ show currentVertexName
                            ++ ", greens: " ++ show (graphVertexNames greens)) $
                            graphGetClosestToVertex yellows currentVertexName greens

                    let closestDistance = howFar closest
                        closestVertexName = neighbour closest
                        nextDistance = closestDistance + currentDistance

                        closestVertex = graphGetVertex yellows' closestVertexName
                        (yellows'', greens'') = 
                                trace ("transferVertexUpdatingAccumulatedDistance called by shortest': yellows: " ++ show (graphVertexNames yellows) ++ ", greens: " ++ show (graphVertexNames greens)
                                ++ ", neighbours:" ++ show ([]::[Neighbour]) ++ ", currentVertexName: " ++ show currentVertexName ++ ", nextDistance: " ++ show nextDistance ) $
                                    transferVertexUpdatingAccumulatedDistance (yellows', greens') [] (fromJust closestVertex) nextDistance NoCompare
        
                    trace ( "shortest' called by shortest': rs: " ++ show (graphVertexNames reds')
                        ++ " ys: " ++ show (graphVertexNames yellows'')
                        ++ " gs: " ++ show (graphVertexNames greens'')
                        ++ " from: " ++ show fromName ++ " to: " ++ show toName ++ " closest: " ++ show closestVertexName
                        ++ " currentDistance: " ++ show nextDistance
                        ++ ", rs full: " ++ show reds' ++ ", ys full: " ++ show yellows' ++ ", gs full: " ++ show greens' ) $
                     shortest' reds' yellows'' greens'' fromName toName nextDistance (fromJust closestVertex)

    shortest'' :: Graph -> Graph -> Graph -> Text -> Text -> Double -> [Neighbour] -> (Graph, Graph, Graph)
    shortest'' reds yellows greens fromName toName currentDistance [] = (reds, yellows, greens)
    shortest'' reds yellows greens fromName toName currentDistance [currentNeighbour] =
        let
            currentVertexName = neighbour currentNeighbour
            closestNeighbour = currentNeighbour
            closestDistance = howFar closestNeighbour
            nextDistance = closestDistance + currentDistance
        in
            do 
                neighbours <- 
                    trace ("graphGetAdmissibleVertexNeighbours called by shortest': greens = " ++ show (graphVertexNames greens) ++ ", currentVertexName = " ++ show currentVertexName ) $
                      graphGetAdmissibleVertexNeighbours greens currentVertexName greens
                if isNothing neighbours
                then (reds, yellows, greens)
                else 
                    let 
                    (reds', yellows') =
                        trace ("tellTheNeighbours called by shortest'': currentVertexName = " ++ show currentVertexName 
                            ++ " reds: " ++ show (graphVertexNames reds)
                            ++ " ys1: " ++ show (graphVertexNames yellows)
                            ++ ", neighbours = " ++ show (neighbourNames (fromJust neighbours)) ++ ", nextDistance = " ++ show currentDistance
                            ++ ", reds full: " ++ show reds ++ ", yellows full: " ++ show yellows ++ ", greens full: " ++ show greens) $
                        tellTheNeighbours (reds, yellows) (fromJust neighbours) currentDistance
                    in
                        (reds', yellows', greens)
             
    shortest'' reds yellows greens fromName toName currentDistance (currentNeighbour: currentNeighbours) = 
        let
            (reds', yellows', greens') = shortest'' reds yellows greens fromName toName currentDistance [currentNeighbour]
        in
            shortest'' reds' yellows' greens' fromName toName currentDistance currentNeighbours

