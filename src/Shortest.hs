{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    shortest
) where

    import Data.Aeson
    import Data.Aeson.Types
    import qualified Data.ByteString.Lazy.Char8 as DBSLC8
    import Data.Either.Unwrap (isLeft, isRight, fromLeft, fromRight)
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

    -- Intermediate data structure
    data Connection = Connection { from :: Text, to :: Text, dist :: Double } deriving (Show)

    -- Manipulation in Dijkstra
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

    -- Input and Output
    data Distance = Distance {distance:: Double} deriving (Show, Generic)
    instance ToJSON Distance
    instance FromJSON Distance

    data StartEnd = StartEnd {start:: String, end:: String} deriving (Show, Generic)
    instance ToJSON StartEnd
    instance FromJSON StartEnd

    -- When updating vertex data
    data OptionalCompare = Compare | NoCompare deriving (Eq, Show)

    sortNeighboursByDistance :: [Neighbour] -> [Neighbour]
    sortNeighboursByDistance = sortBy (comparing howFar)

    sortVerticesByDistance :: [Vertex] -> [Vertex]
    sortVerticesByDistance = sortBy (comparing accumulatedDistance)

    getVertexNames :: [Connection] -> [Text]
    getVertexNames = nub . Prelude.map from

    pullVertex :: Text -> [Connection] -> Double -> Vertex
    pullVertex vertex cs accumulatedDistance =
        let
            rawVertices = filter (\x -> from x == vertex) cs
            neighbours = Prelude.map (\x -> Neighbour {
                 neighbour = Shortest.to x
                 , howFar = Shortest.dist x
                 }) rawVertices
        in
            Vertex {
                vertex = vertex
                , accumulatedDistance = accumulatedDistance
                , neighbours = sortNeighboursByDistance neighbours
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
             Shortest.dist = MapDefinitions.distance destination }] 
         ++ [Connection {
            Shortest.from = MapDefinitions.to destination,
            Shortest.to = placeName,
            Shortest.dist = MapDefinitions.distance destination }]
    expandPlace' placeName (d : destinations) connections =
        connections
        ++ [Connection {
            Shortest.from = placeName,
            Shortest.to = MapDefinitions.to d,
            Shortest.dist = MapDefinitions.distance d }] 
        ++ [Connection {
            Shortest.from = MapDefinitions.to d,
            Shortest.to = placeName,
            Shortest.dist = MapDefinitions.distance d }]
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
                    -- trace ("getNeighbour called by neighbourHowFarByName: ns: " ++ show ns ++ ", name: " ++ show name) $
                    getNeighbour ns name
            in
                maybe (error "neighbourHowFarByName: Error: Unexpected Nothing returned by getNeighbour.") howFar n

    graphGetAdmissibleVertexNeighbours :: Graph -> Text -> Graph -> Maybe [Neighbour]
    graphGetAdmissibleVertexNeighbours g currentVertexName greens =
        let
            gvs = vertices greens
        in
            if
                null gvs
            then 
                graphGetVertexNeighbours g currentVertexName
            else
                let
                    greenNames = Prelude.map vertex gvs
                in
                    do
                        ns <- graphGetVertexNeighbours g currentVertexName
                        return $ deleteNeighboursByName ns greenNames
    
    graphGetClosestToVertex :: Graph -> Text -> Graph -> Maybe Neighbour
    graphGetClosestToVertex g vertex greens =
        fmap head (graphGetAdmissibleVertexNeighbours g vertex greens)

    graphGetMinimumYellowByDistance :: Graph -> Vertex
    graphGetMinimumYellowByDistance g =
        head (vertices g)
        
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
    insertVertex vs v = 
        sortVerticesByDistance (v:vs)

    graphInsertVertex  :: Graph -> Vertex -> Graph
    graphInsertVertex pg vertex =
        Graph { vertices = insertVertex (vertices pg) vertex }
    
    compareOrNot :: Double -> Double -> OptionalCompare -> Double
    compareOrNot oldValue valueForConsideration compare =
            oldValue + valueForConsideration 

    transferVerticesUpdatingAccumulatedDistance :: (Graph, Graph) -> [Neighbour] -> [Text] -> Double -> OptionalCompare -> (Graph, Graph)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) _ [] _ _ =
        (graph1, graph2)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) neighbours [vName] currentDistance compare =
        -- trace ("transferVerticesUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1: " ++ show (graphVertexNames graph1) ++ ", graph2: " ++ show (graphVertexNames graph2)
        -- ++ ", neighbours:" ++ show neighbours ++ ", vName: " ++ show vName ++ ", currentDistance: " ++ show currentDistance) $
           transferVertexUpdatingAccumulatedDistance (graph1, graph2) neighbours vName currentDistance compare
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) neighbours (vName:vNames) currentDistance compare =
        let 
            (graph1', graph2') =
                -- trace ("transferVertexUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1: " ++ show (graphVertexNames graph1) ++ ", graph2: " ++ show (graphVertexNames graph2)
                --  ++ ", neighbours:" ++ show neighbours ++ ", vName: " ++ show vName ++ ", currentDistance: " ++ show currentDistance) $
                transferVertexUpdatingAccumulatedDistance (graph1, graph2) neighbours vName currentDistance compare
        in
            -- trace ("transferVerticesUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1': " ++ show (graphVertexNames graph1') ++ ", graph2': " ++ show (graphVertexNames graph2')
            -- ++ ", neighbours: " ++ show neighbours ++ ", vNames: " ++ show vNames ++ ", currentDistance: " ++ show currentDistance) $
            transferVerticesUpdatingAccumulatedDistance (graph1', graph2') neighbours vNames currentDistance compare

    -- when accumulating we need the minimum current distance for this neighbour
    pickMinimumAccumulatedDistance :: Double -> Double -> Double -> OptionalCompare -> Double
    pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance compare =
        if compare == Compare
            then
                min accumulatedD (currentDistance + neighbourDistance)
            else
                currentDistance

    transferVertexUpdatingAccumulatedDistance :: (Graph, Graph) -> [Neighbour] -> Text -> Double -> OptionalCompare -> (Graph, Graph)
    transferVertexUpdatingAccumulatedDistance (graph1, graph2) neighbours_in currentVName currentDistance compare =
        let
            txVertex = graphGetVertex graph1 currentVName
        in
            if isNothing txVertex
            then (graph1, graph2)
            else
                let 
                    txV = fromJust txVertex
                    accumulatedD = accumulatedDistance txV
                    neighbourDistance =
                        -- trace ("neighbourHowFarByName called by transferVertexUpdatingAccumulatedDistance: neighbours_in: " ++ show neighbours_in
                        --  ++ ", currentVName: " ++ show currentVName)$ 
                        neighbourHowFarByName neighbours_in currentVName
                    
                    accumulatedD' = 
                        -- trace ("pickMinimumAccumulatedDistance called by transferVertexUpdatingAccumulatedDistance: accumulatedD: " ++ show accumulatedD
                        -- ++ ", neighbourDistance: " ++ show neighbourDistance ++ ", currentDistance: " ++ show currentDistance ++ ", compare: " ++ show compare) $
                        pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance compare
                    v =  Vertex {
                        vertex = vertex txV
                        , accumulatedDistance = accumulatedD'
                        , neighbours = neighbours txV
                    }
                    
                    graph2' = graphDeleteVertex graph2 txV
                    newGraph1 = graphDeleteVertex graph1 txV
                    newGraph2 = 
                    --    trace ("graphInsertVertex called by transferVertexUpdatingAccumulatedDistance: graph2': " ++ show (graphVertexNames graph2')
                    --          ++ ", v: " ++ show v
                    --          ++ ", txV: " ++ show txV
                    --        ) $
                              graphInsertVertex graph2' v
                in
                    (newGraph1, newGraph2) 

    transferVertex :: (Graph, Graph) -> Text -> (Graph, Graph)
    transferVertex (graph1, graph2) vName =
        let
            txVertex = graphGetVertex graph1 vName
        in
            if isNothing txVertex
            then (graph1, graph2)
            else
                let 
                    txV = fromJust txVertex
                    newGraph1 = graphDeleteVertex graph1 txV
                    newGraph2 = 
                        graphInsertVertex graph2 txV
                in
                    (newGraph1, newGraph2) 

    tellTheNeighbours :: Text -> (Graph, Graph) -> [Neighbour] -> Double -> (Graph, Graph)
    tellTheNeighbours vertexName (reds, yellows) ns currentDistance =
        let
            nNames = Prelude.map neighbour ns

            redNeighbours = mapMaybe (graphGetVertex reds) nNames
            redNeighbourNames = Prelude.map vertex redNeighbours
            (rs', ys') = 
                -- trace ("transferVerticesUpdatingAccumulatedDistance called by TTN: reds: " ++ show (graphVertexNames reds) ++ ", yellows: " ++ show (graphVertexNames yellows)
                --     ++ ", neighbours: " ++ show (neighbourNames ns)
                --     ++ ", redNeighbourNames: " ++ show redNeighbourNames ++ ", currentDistance: " ++ show currentDistance ++ ", compare: " ++ show Compare
                --     ++ ", reds full: " ++ show reds ++ ", yellows full: " ++ show yellows)
                transferVerticesUpdatingAccumulatedDistance (reds, yellows) ns redNeighbourNames currentDistance Compare

            yellowNeighbours = mapMaybe (graphGetVertex ys') nNames
            yellowNeighbourNames = Prelude.map vertex yellowNeighbours
            (_, ys'') = 
                -- trace ("transferVerticesUpdatingAccumulatedDistance called by TTN: ys' x 2: " ++ show (graphVertexNames ys') -- ++ ", ys': " ++ show (graphVertexNames ys')
                -- ++ ", neighbours: " ++ show (neighbourNames ns)
                -- ++ ", yellowNeighbourNames: " ++ show yellowNeighbourNames ++ ", currentDistance: " ++ show currentDistance ++ ", compare: " ++ show Compare
                -- ++ ", yellows full: " ++ show ys')
                transferVerticesUpdatingAccumulatedDistance (ys', ys') ns yellowNeighbourNames currentDistance Compare
        in 
            (rs', ys'')

    vertexNames :: [Vertex] -> [Text]
    vertexNames = Prelude.map vertex

    -- vertexAccumulatedDistances :: [Vertex] -> [Text]
    -- vertexAccumulatedDistances = Prelude.map accumulatedDistance

    graphVertexNames :: Graph -> [Text]
    graphVertexNames g = vertexNames (vertices g)

    neighbourNames :: [Neighbour] -> [Text]
    neighbourNames = Prelude.map neighbour

    -- add :: String -> IO ()
    -- add candidateVertex =
    --     do 
    --         m <- readMap
    --         let pg = mapToGraph m

    readStartEndFromString :: String -> StartEnd
    readStartEndFromString candidateStartEnd =
        let
            eitherStartEnd = eitherDecode (DBSLC8.pack candidateStartEnd) :: (Either String StartEnd)
        in
            if isLeft eitherStartEnd 
                then 
                    error ( "readStartEndFromString: Input [" ++ candidateStartEnd ++ "] is not valid.")
                else
                    fromRight eitherStartEnd
        

    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            let pg = mapToGraph m
                currentDistance = 0
                (reds, yellows) =
                    -- trace ("transferVertexUpdatingAccumulatedDistance called by shortest: pg: " ++ show (graphVertexNames pg) ++ ", graph2: " ++ show (graphVertexNames Graph{vertices = []})
                    -- ++ ", neighbours:" ++ show ([]::[Neighbour]) ++ ", vName: " ++ show from ++ ", nextDistance: " ++ show 0) $
                    transferVertexUpdatingAccumulatedDistance (pg, Graph{vertices = []}) [] (pack from) currentDistance NoCompare
                greens = Graph{vertices = []}
                distance = 
                --   trace ( "shortest' called by shortest: rs: " ++ show (graphVertexNames reds)
                --     ++ ", ys: " ++ show (graphVertexNames yellows)
                --     ++ ", gs: " ++ show (graphVertexNames greens)
                --     ++ ", from: " ++ from ++ ", to: " ++ to ++ ", closest: " ++ from
                --     ++ ", currentDistance: " ++ show currentDistance  ++ ", ys full: " ++ show yellows ++ ", gs full: " ++ show greens )
                    shortest' reds yellows greens (pack from) (pack to) (pack from) currentDistance
            print ("Shortest distance from [" ++ from ++ "] to [" ++ to ++ "] = " ++ show distance)

    shortest' :: Graph -> Graph -> Graph -> Text -> Text -> Text -> Double -> Double
    shortest' reds yellows greens fromName toName currentVertexName currentDistance = 
        if currentVertexName == toName
        then
            let
                vy = graphGetVertex yellows toName
                distance = if isNothing vy
                            then 
                                let vg = graphGetVertex greens toName
                                in if isNothing vg
                                    then error "shortest': Error: Destination vertex was not found in either yellows or greens unexpectedly."
                                    else min currentDistance (accumulatedDistance $ fromJust vg)
                            else min currentDistance (accumulatedDistance $ fromJust vy)
            in
                distance
        else
            let currentVertex = graphGetMinimumYellowByDistance yellows
                currentDistance = accumulatedDistance currentVertex
                currentVertexName = vertex currentVertex
                (ys1, gs1) = 
                    -- trace ("transferVertex called by shortest': yellows: " ++ show (graphVertexNames yellows) ++ ", greens: " ++ show (graphVertexNames greens)
                    --  ++ ", currentVertexName: " ++ show currentVertexName
                    --  ++ ", yellows full: " ++ show yellows ++ ", greens full: " ++ show greens ) $
                        transferVertex (yellows, greens) currentVertexName
                neighbours = 
                    -- trace ("graphGetAdmissibleVertexNeighbours called by shortest': gs1 = " ++ show (graphVertexNames gs1) ++ ", currentVertexName = " ++ show currentVertexName ) $
                        graphGetAdmissibleVertexNeighbours gs1 currentVertexName gs1
                (rs2, ys2) =
                    -- trace ("tellTheNeighbours called by shortest': currentVertexName = " ++ show currentVertexName 
                    --     ++ " reds: " ++ show (graphVertexNames reds)
                    --     ++ " ys1: " ++ show (graphVertexNames ys1)
                    --     ++ ", neighbours = " ++ show (neighbourNames (fromJust neighbours)) ++ ", nextDistance = " ++ show currentDistance
                    --     ++ ", reds full: " ++ show reds ++ ", ys1 full: " ++ show ys1 ++ ", gs1 full: " ++ show gs1) $
                    tellTheNeighbours currentVertexName (reds, ys1) (fromJust neighbours) currentDistance
                in
                -- trace ( "shortest' called by shortest': rs: " ++ show (graphVertexNames rs2)
                --         ++ " ys: " ++ show (graphVertexNames ys2)
                --         ++ " gs: " ++ show (graphVertexNames gs1)
                --         ++ " from: " ++ show fromName ++ " to: " ++ show toName ++ " current: " ++ show currentVertexName
                --         ++ " currentDistance: " ++ show currentDistance
                --         ++ ", rs full: " ++ show rs2 ++ ", ys full: " ++ show ys2 ++ ", gs full: " ++ show gs1 ) $
                    shortest' rs2 ys2 gs1 fromName toName currentVertexName currentDistance
