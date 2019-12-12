{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Shortest (
    UnusualResult(..)
    , Distance(..)
    , dijkstra
) where
    
import Data.Aeson (eitherDecode, ToJSON, FromJSON)
import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
import Data.Maybe (fromJust, mapMaybe, isNothing)
import Data.Ord (min)
import Data.String.Conversions (cs)
import Data.Text (Text, pack, unpack)
import GHC.Generics hiding (from, to)
import Distance
import Graph
import GraphOperations
import MapDefinitions ( readMap, mapToGraph )
import StartEnd
import Neighbour
import Vertex

-- When updating vertex data
data OptionalCompare = Compare | NoCompare deriving (Eq, Show)

data UnusualResult = NegativeRoadLength | NotConnected Text Text deriving (Show, Generic, Eq)
instance ToJSON UnusualResult
instance FromJSON UnusualResult

readStartEndFromString :: Text -> StartEnd
readStartEndFromString candidateStartEnd =
    case eitherStartEnd of
    Left _ -> error ( "readStartEndFromString: Input [" ++ cs candidateStartEnd ++ "] is not valid.")
    Right r -> r
    where
        eitherStartEnd = eitherDecode $ cs candidateStartEnd :: (Either String StartEnd)
         
transferVerticesUpdatingAccumulatedDistance :: [Neighbour] -> [Text] -> Double -> OptionalCompare -> (Graph, Graph) -> (Graph, Graph)
transferVerticesUpdatingAccumulatedDistance _ [] _ _ (graph1, graph2) = (graph1, graph2)
transferVerticesUpdatingAccumulatedDistance ns [vName] currentDistance optCompare (graph1, graph2) =
       transferVertexUpdatingAccumulatedDistance ns vName currentDistance optCompare (graph1, graph2)
transferVerticesUpdatingAccumulatedDistance ns (vName:vNames) currentDistance optCompare (graph1, graph2) =
    transferVerticesUpdatingAccumulatedDistance ns vNames currentDistance optCompare (graph1', graph2')
    where
        (graph1', graph2') =
            transferVertexUpdatingAccumulatedDistance ns vName currentDistance optCompare (graph1, graph2)

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
    if null $ vertices g
    then Left $ NotConnected from to
    else Right $ head (vertices g)

graphGetAdmissibleVertexNeighbours :: Graph -> Text -> Graph -> Maybe [Neighbour]
graphGetAdmissibleVertexNeighbours g currentVertexName greens =
    if null gvs
    then  graphGetVertexNeighbours g currentVertexName
    else do
            ns <- graphGetVertexNeighbours g currentVertexName
            return $ deleteNeighboursByName greenNames ns
    where
        gvs = vertices greens
        greenNames = Prelude.map vertex gvs

transferVertexUpdatingAccumulatedDistance :: [Neighbour] -> Text -> Double -> OptionalCompare -> (Graph, Graph) -> (Graph, Graph)
transferVertexUpdatingAccumulatedDistance neighboursIn currentVName currentDistance optCompare (graph1, graph2) =
  case graphGetVertex graph1 currentVName of
    Nothing -> (graph1, graph2)
    Just txV -> (graphDeleteVertex graph1 txV, graphInsertVertex graph2' v) 
        where
            accumulatedD = accumulatedDistance txV
            neighbourDistance = neighbourHowFarByName neighboursIn currentVName
            accumulatedD' = pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance optCompare
            v =  Vertex {
                vertex = vertex txV
                , accumulatedDistance = accumulatedD'
                , neighbours = neighbours txV
            }
            graph2' = graphDeleteVertex graph2 txV

transferVertex :: Text -> (Graph, Graph) -> (Graph, Graph)
transferVertex vName (graph1, graph2) =
  case graphGetVertex graph1 vName of
    Nothing -> (graph1, graph2)
    Just vertex -> (graphDeleteVertex graph1 vertex
                    , graphInsertVertex graph2 vertex)

tellTheNeighbours :: [Neighbour] -> Double -> (Graph, Graph) -> (Graph, Graph)
tellTheNeighbours ns currentDistance (reds, yellows) =
    (rs', ys'')
    where
        nNames = Prelude.map neighbourName ns
        redNeighbours = mapMaybe (graphGetVertex reds) nNames
        redNeighbourNames = Prelude.map vertex redNeighbours
        (rs', ys') = transferVerticesUpdatingAccumulatedDistance ns redNeighbourNames currentDistance Compare  (reds, yellows)
        yellowNeighbours = mapMaybe (graphGetVertex ys') nNames
        yellowNeighbourNames = Prelude.map vertex yellowNeighbours
        (_, ys'') = transferVerticesUpdatingAccumulatedDistance ns yellowNeighbourNames currentDistance Compare (ys', ys')

dijkstra :: Text -> IO (Either UnusualResult Distance)
dijkstra couldBeStartEnd =
    do 
        m <- readMap
        let pg = mapToGraph m
            startEnd = readStartEndFromString couldBeStartEnd
            from = start startEnd
            to = end startEnd
            currentDistance = 0
            (reds, yellows) = transferVertexUpdatingAccumulatedDistance [] from currentDistance NoCompare (pg, Graph{vertices = []})
            greens = Graph{vertices = []}
            shortestDistanceByDijkstra = dijkstra' reds yellows greens from to from currentDistance
            theResult = if isLeft shortestDistanceByDijkstra
                        then Left $ fromLeft shortestDistanceByDijkstra
                        else Right Distance {distance = fromRight shortestDistanceByDijkstra}
        return theResult

dijkstra' :: Graph -> Graph -> Graph -> Text -> Text -> Text -> Double -> Either UnusualResult Double
dijkstra' reds yellows greens fromName toName currentVertexName currentDistance
    | currentVertexName == toName =
        if isNothing vy
        then 
            if isNothing vg
            then error "dijkstra': Error: Destination vertex was not found in either yellows or greens unexpectedly."
            else Right (min currentDistance (accumulatedDistance $ fromJust vg))
        else Right (min currentDistance (accumulatedDistance $ fromJust vy))
    | otherwise =
        if isLeft eitherCurrentVertex
        then Left (fromLeft eitherCurrentVertex)
        else
            dijkstra' rs2 ys2 gs1 fromName toName newCurrentVertexName newCurrentDistance
    where
        vy = graphGetVertex yellows toName
        vg = graphGetVertex greens toName
        eitherCurrentVertex = graphGetMinimumYellowByDistance yellows fromName toName
        currentVertex = fromRight eitherCurrentVertex
        newCurrentDistance = accumulatedDistance currentVertex
        newCurrentVertexName = vertex currentVertex
        (ys1, gs1) = transferVertex newCurrentVertexName (yellows, greens)
        ns = graphGetAdmissibleVertexNeighbours gs1 newCurrentVertexName gs1
        (rs2, ys2) = tellTheNeighbours (fromJust ns) newCurrentDistance (reds, ys1)
