{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    UnusualResult(..)
    , dijkstra
) where

    import Data.Aeson (ToJSON, FromJSON)
    import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
    import Data.Maybe (fromJust, mapMaybe, isNothing)
    import Data.Ord (min)
    import Data.Text (Text, pack, unpack)
--    import Debug.Trace
    import GHC.Generics hiding (from, to)
    import Graph (
        mapToGraph
        , deleteNeighboursByName
        , readStartEndFromString
        , graphGetVertex
        , graphGetVertexNeighbours
        , neighbourHowFarByName
        , graphDeleteVertex
        , graphInsertVertex
        , Graph(Graph), vertices
        , Vertex(Vertex), vertex, accumulatedDistance, neighbours
        , Neighbour(), neighbour
        , OptionalCompare(Compare, NoCompare)
        , start, end
        , Distance(..))

    import MapDefinitions ( readMap )
    import Numeric.Natural ()

    data UnusualResult = NegativeRoadLength | NotConnected String String deriving (Show, Generic, Eq)
    instance ToJSON UnusualResult
    instance FromJSON UnusualResult

    transferVerticesUpdatingAccumulatedDistance :: (Graph, Graph) -> [Neighbour] -> [Text] -> Double -> OptionalCompare -> (Graph, Graph)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) _ [] _ _ =
        (graph1, graph2)
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) ns [vName] currentDistance optCompare =
        -- trace ("transferVerticesUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1: " ++ show (graphVertexNames graph1) ++ ", graph2: " ++ show (graphVertexNames graph2)
        -- ++ ", neighbours:" ++ show neighbours ++ ", vName: " ++ show vName ++ ", currentDistance: " ++ show currentDistance) $
           transferVertexUpdatingAccumulatedDistance (graph1, graph2) ns vName currentDistance optCompare
    transferVerticesUpdatingAccumulatedDistance (graph1, graph2) ns (vName:vNames) currentDistance optCompare =
        let 
            (graph1', graph2') =
                -- trace ("transferVertexUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1: " ++ show (graphVertexNames graph1) ++ ", graph2: " ++ show (graphVertexNames graph2)
                --  ++ ", neighbours:" ++ show neighbours ++ ", vName: " ++ show vName ++ ", currentDistance: " ++ show currentDistance) $
                transferVertexUpdatingAccumulatedDistance (graph1, graph2) ns vName currentDistance optCompare
        in
            -- trace ("transferVerticesUpdatingAccumulatedDistance called by transferVerticesUpdatingAccumulatedDistance: graph1': " ++ show (graphVertexNames graph1') ++ ", graph2': " ++ show (graphVertexNames graph2')
            -- ++ ", neighbours: " ++ show neighbours ++ ", vNames: " ++ show vNames ++ ", currentDistance: " ++ show currentDistance) $
            transferVerticesUpdatingAccumulatedDistance (graph1', graph2') ns vNames currentDistance optCompare

    -- when accumulating we need the minimum current distance for this neighbour
    pickMinimumAccumulatedDistance :: Double -> Double -> Double -> OptionalCompare -> Double
    pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance optCompare =
        if optCompare == Compare
            then
                min accumulatedD (currentDistance + neighbourDistance)
            else
                currentDistance

    graphGetMinimumYellowByDistance :: Graph -> Text -> Text -> Either UnusualResult Vertex
    graphGetMinimumYellowByDistance g from to=
        let vs = vertices g
        in
            if null vs
                then Left $ NotConnected (unpack from) (unpack to)
                else Right $ head (vertices g)
        
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
    
    transferVertexUpdatingAccumulatedDistance :: (Graph, Graph) -> [Neighbour] -> Text -> Double -> OptionalCompare -> (Graph, Graph)
    transferVertexUpdatingAccumulatedDistance (graph1, graph2) neighbours_in currentVName currentDistance optCompare =
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
                        pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance optCompare
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

    tellTheNeighbours :: (Graph, Graph) -> [Neighbour] -> Double -> (Graph, Graph)
    tellTheNeighbours (reds, yellows) ns currentDistance =
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

    dijkstra :: String -> IO (Either UnusualResult Distance)
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
                theResult = if isLeft shortestDistanceByDijkstra
                            then Left $ fromLeft shortestDistanceByDijkstra
                            else Right Distance {Graph.distance = fromRight shortestDistanceByDijkstra}
            return theResult

    dijkstra' :: Graph -> Graph -> Graph -> Text -> Text -> Text -> Double -> Either UnusualResult Double
    dijkstra' reds yellows greens fromName toName currentVertexName currentDistance = 
        if currentVertexName == toName
        then
            let
                vy = graphGetVertex yellows toName
            in
                if isNothing vy
                then 
                    let vg = graphGetVertex greens toName
                    in if isNothing vg
                        then error "dijkstra': Error: Destination vertex was not found in either yellows or greens unexpectedly."
                        else Right (min currentDistance (accumulatedDistance $ fromJust vg))
                else Right (min currentDistance (accumulatedDistance $ fromJust vy))
        else
            let eitherCurrentVertex = graphGetMinimumYellowByDistance yellows fromName toName
            in
                if isLeft eitherCurrentVertex
                then Left (fromLeft eitherCurrentVertex)
                else
                    let
                        currentVertex = fromRight eitherCurrentVertex
                        newCurrentDistance = accumulatedDistance currentVertex
                        newCurrentVertexName = vertex currentVertex
                        (ys1, gs1) = 
                            -- trace ("transferVertex called by dijkstra': yellows: " ++ show (graphVertexNames yellows) ++ ", greens: " ++ show (graphVertexNames greens)
                            --  ++ ", newCurrentVertexName: " ++ show newCurrentVertexName
                            --  ++ ", yellows full: " ++ show yellows ++ ", greens full: " ++ show greens ) $
                                transferVertex (yellows, greens) newCurrentVertexName
                        ns = 
                            -- trace ("graphGetAdmissibleVertexNeighbours called by dijkstra': gs1 = " ++ show (graphVertexNames gs1) ++ ", newCurrentVertexName = " ++ show newCurrentVertexName ) $
                                graphGetAdmissibleVertexNeighbours gs1 newCurrentVertexName gs1
                        (rs2, ys2) =
                            -- trace ("tellTheNeighbours called by dijkstra': newCurrentVertexName = " ++ show newCurrentVertexName 
                            --     ++ " reds: " ++ show (graphVertexNames reds)
                            --     ++ " ys1: " ++ show (graphVertexNames ys1)
                            --     ++ ", neighbours = " ++ show (neighbourNames (fromJust neighbours)) ++ ", nextDistance = " ++ show newCurrentDistance
                            --     ++ ", reds full: " ++ show reds ++ ", ys1 full: " ++ show ys1 ++ ", gs1 full: " ++ show gs1) $
                            tellTheNeighbours (reds, ys1) (fromJust ns) newCurrentDistance
                        in
                        -- trace ( "dijkstra' called by dijkstra': rs: " ++ show (graphVertexNames rs2)
                        --         ++ " ys: " ++ show (graphVertexNames ys2)
                        --         ++ " gs: " ++ show (graphVertexNames gs1)
                        --         ++ " from: " ++ show fromName ++ " to: " ++ show toName ++ " current: " ++ show newCurrentVertexName
                        --         ++ " newCurrentDistance: " ++ show newCurrentDistance
                        --         ++ ", rs full: " ++ show rs2 ++ ", ys full: " ++ show ys2 ++ ", gs full: " ++ show gs1 ) $
                            dijkstra' rs2 ys2 gs1 fromName toName newCurrentVertexName newCurrentDistance
