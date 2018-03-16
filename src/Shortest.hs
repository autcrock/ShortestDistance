module Shortest (
    dijkstra
) where

    import Data.Aeson (eitherDecode, encode, ToJSON, FromJSON)
    import Data.Either.Unwrap (isLeft, isRight, fromLeft, fromRight)
    import Data.List (sortBy, nub, find, delete, deleteBy)
    import Data.Maybe (fromJust, fromMaybe, mapMaybe, isNothing)
    import Data.Ord (min)
    import Data.Text (Text, pack)
--    import Debug.Trace
    import GHC.Generics hiding (from, to)
    import Intermediate(
        mapToGraph
        , readStartEndFromString
        , sortVerticesByDistance
        , graphGetVertex
        , neighbourHowFarByName
        , graphDeleteVertex
        , graphInsertVertex
        , graphGetMinimumYellowByDistance
        , graphGetAdmissibleVertexNeighbours
        , Graph(Graph), vertices
        , Vertex(Vertex), vertex, accumulatedDistance, neighbours
        , Neighbour(Neighbour), neighbour, howFar
        , OptionalCompare(Compare, NoCompare)
        , StartEnd(StartEnd), start, end
        , Distance(..))

    import MapDefinitions (
        Map, Place, Destination, place, destinations, distance, map, readMap, to
        )
    import Numeric.Natural (Natural)

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

    dijkstra :: String -> IO Distance
    dijkstra couldBeStartEnd =
        do 
            m <- readMap
--            print ("dijkstra: couldBeStartEnd: " ++ couldBeStartEnd)
            let pg = mapToGraph m
                startEnd = readStartEndFromString couldBeStartEnd
                from = start startEnd
                to = end startEnd
                currentDistance = 0
                (reds, yellows) =
                    -- trace ("transferVertexUpdatingAccumulatedDistance called by shortest: pg: " ++ show (graphVertexNames pg) ++ ", graph2: " ++ show (graphVertexNames Graph{vertices = []})
                    -- ++ ", neighbours:" ++ show ([]::[Neighbour]) ++ ", vName: " ++ show from ++ ", nextDistance: " ++ show 0) $
                    transferVertexUpdatingAccumulatedDistance (pg, Graph{vertices = []}) [] (pack from) currentDistance NoCompare
                greens = Graph{vertices = []}
                shortestDistanceByDijkstra = 
                --   trace ( "dijkstra' called by shortest: rs: " ++ show (graphVertexNames reds)
                --     ++ ", ys: " ++ show (graphVertexNames yellows)
                --     ++ ", gs: " ++ show (graphVertexNames greens)
                --     ++ ", from: " ++ from ++ ", to: " ++ to ++ ", closest: " ++ from
                --     ++ ", currentDistance: " ++ show currentDistance  ++ ", ys full: " ++ show yellows ++ ", gs full: " ++ show greens )
                    dijkstra' reds yellows greens (pack from) (pack to) (pack from) currentDistance
            return Distance {Intermediate.distance = shortestDistanceByDijkstra}

    dijkstra' :: Graph -> Graph -> Graph -> Text -> Text -> Text -> Double -> Double
    dijkstra' reds yellows greens fromName toName currentVertexName currentDistance = 
        if currentVertexName == toName
        then
            let
                vy = graphGetVertex yellows toName
                distance = if isNothing vy
                            then 
                                let vg = graphGetVertex greens toName
                                in if isNothing vg
                                    then error "dijkstra': Error: Destination vertex was not found in either yellows or greens unexpectedly."
                                    else min currentDistance (accumulatedDistance $ fromJust vg)
                            else min currentDistance (accumulatedDistance $ fromJust vy)
            in
                distance
        else
            let currentVertex = graphGetMinimumYellowByDistance yellows
                currentDistance = accumulatedDistance currentVertex
                currentVertexName = vertex currentVertex
                (ys1, gs1) = 
                    -- trace ("transferVertex called by dijkstra': yellows: " ++ show (graphVertexNames yellows) ++ ", greens: " ++ show (graphVertexNames greens)
                    --  ++ ", currentVertexName: " ++ show currentVertexName
                    --  ++ ", yellows full: " ++ show yellows ++ ", greens full: " ++ show greens ) $
                        transferVertex (yellows, greens) currentVertexName
                neighbours = 
                    -- trace ("graphGetAdmissibleVertexNeighbours called by dijkstra': gs1 = " ++ show (graphVertexNames gs1) ++ ", currentVertexName = " ++ show currentVertexName ) $
                        graphGetAdmissibleVertexNeighbours gs1 currentVertexName gs1
                (rs2, ys2) =
                    -- trace ("tellTheNeighbours called by dijkstra': currentVertexName = " ++ show currentVertexName 
                    --     ++ " reds: " ++ show (graphVertexNames reds)
                    --     ++ " ys1: " ++ show (graphVertexNames ys1)
                    --     ++ ", neighbours = " ++ show (neighbourNames (fromJust neighbours)) ++ ", nextDistance = " ++ show currentDistance
                    --     ++ ", reds full: " ++ show reds ++ ", ys1 full: " ++ show ys1 ++ ", gs1 full: " ++ show gs1) $
                    tellTheNeighbours currentVertexName (reds, ys1) (fromJust neighbours) currentDistance
                in
                -- trace ( "dijkstra' called by dijkstra': rs: " ++ show (graphVertexNames rs2)
                --         ++ " ys: " ++ show (graphVertexNames ys2)
                --         ++ " gs: " ++ show (graphVertexNames gs1)
                --         ++ " from: " ++ show fromName ++ " to: " ++ show toName ++ " current: " ++ show currentVertexName
                --         ++ " currentDistance: " ++ show currentDistance
                --         ++ ", rs full: " ++ show rs2 ++ ", ys full: " ++ show ys2 ++ ", gs full: " ++ show gs1 ) $
                    dijkstra' rs2 ys2 gs1 fromName toName currentVertexName currentDistance
