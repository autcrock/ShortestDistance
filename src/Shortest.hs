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
import Data.Text (Text, pack, unpack)
import Data.String.Conversions (cs)
import GHC.Generics hiding (from, to)
import Graph (
    deleteNeighboursByName
    , graphGetVertex
    , graphGetVertexNeighbours
    , neighbourHowFarByName
    , graphDeleteVertex
    , graphInsertVertex
    , Graph(Graph), vertices
    , Vertex(Vertex), vertex, accumulatedDistance, neighbours
    , Neighbour(), neighbourName
    )

import MapDefinitions ( readMap, mapToGraph, StartEnd(..) )

newtype Distance = Distance{distance :: Double} deriving (Show, Generic, Eq)
instance ToJSON Distance
instance FromJSON Distance

-- When updating vertex data
data OptionalCompare = Compare | NoCompare deriving (Eq, Show)

data UnusualResult = NegativeRoadLength | NotConnected Text Text deriving (Show, Generic, Eq)
instance ToJSON UnusualResult
instance FromJSON UnusualResult

readStartEndFromString :: Text -> StartEnd
readStartEndFromString candidateStartEnd =
    let eitherStartEnd = eitherDecode $ cs candidateStartEnd :: (Either String StartEnd)
    in case eitherStartEnd of
        Left _ -> error ( "readStartEndFromString: Input [" ++ cs candidateStartEnd ++ "] is not valid.")
        Right r -> r

transferVerticesUpdatingAccumulatedDistance :: [Neighbour] -> [Text] -> Double -> OptionalCompare -> (Graph, Graph) -> (Graph, Graph)
transferVerticesUpdatingAccumulatedDistance _ [] _ _ (graph1, graph2) = (graph1, graph2)
transferVerticesUpdatingAccumulatedDistance ns [vName] currentDistance optCompare (graph1, graph2) =
       transferVertexUpdatingAccumulatedDistance ns vName currentDistance optCompare (graph1, graph2)
transferVerticesUpdatingAccumulatedDistance ns (vName:vNames) currentDistance optCompare (graph1, graph2) =
    let 
        (graph1', graph2') =
            transferVertexUpdatingAccumulatedDistance ns vName currentDistance optCompare (graph1, graph2)
    in
        transferVerticesUpdatingAccumulatedDistance ns vNames currentDistance optCompare (graph1', graph2')

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

transferVertexUpdatingAccumulatedDistance :: [Neighbour] -> Text -> Double -> OptionalCompare -> (Graph, Graph) -> (Graph, Graph)
transferVertexUpdatingAccumulatedDistance neighboursIn currentVName currentDistance optCompare (graph1, graph2) =
    let
        txVertex = graphGetVertex graph1 currentVName
    in
        if isNothing txVertex
        then (graph1, graph2)
        else
            let 
                txV = fromJust txVertex
                accumulatedD = accumulatedDistance txV
                neighbourDistance = neighbourHowFarByName neighboursIn currentVName
                accumulatedD' = pickMinimumAccumulatedDistance accumulatedD neighbourDistance currentDistance optCompare
                v =  Vertex {
                    vertex = vertex txV
                    , accumulatedDistance = accumulatedD'
                    , neighbours = neighbours txV
                }
                graph2' = graphDeleteVertex graph2 txV
                newGraph1 = graphDeleteVertex graph1 txV
                newGraph2 = graphInsertVertex graph2' v
            in
                (newGraph1, newGraph2) 

transferVertex :: Text -> (Graph, Graph) -> (Graph, Graph)
transferVertex vName (graph1, graph2) =
    let
        txVertex = graphGetVertex graph1 vName
    in
        if isNothing txVertex
        then (graph1, graph2)
        else
            let
                txV = fromJust txVertex
                newGraph1 = graphDeleteVertex graph1 txV
                newGraph2 = graphInsertVertex graph2 txV
            in
                (newGraph1, newGraph2) 

tellTheNeighbours :: [Neighbour] -> Double -> (Graph, Graph) -> (Graph, Graph)
tellTheNeighbours ns currentDistance (reds, yellows) =
    let
        nNames = Prelude.map neighbourName ns

        redNeighbours = mapMaybe (graphGetVertex reds) nNames
        redNeighbourNames = Prelude.map vertex redNeighbours
        (rs', ys') = transferVerticesUpdatingAccumulatedDistance ns redNeighbourNames currentDistance Compare  (reds, yellows)

        yellowNeighbours = mapMaybe (graphGetVertex ys') nNames
        yellowNeighbourNames = Prelude.map vertex yellowNeighbours
        (_, ys'') = transferVerticesUpdatingAccumulatedDistance ns yellowNeighbourNames currentDistance Compare (ys', ys')
    in 
        (rs', ys'')

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
    | otherwise =
        let eitherCurrentVertex = graphGetMinimumYellowByDistance yellows fromName toName
        in
            if isLeft eitherCurrentVertex
            then Left (fromLeft eitherCurrentVertex)
            else
                let
                    currentVertex = fromRight eitherCurrentVertex
                    newCurrentDistance = accumulatedDistance currentVertex
                    newCurrentVertexName = vertex currentVertex
                    (ys1, gs1) = transferVertex newCurrentVertexName (yellows, greens)
                    ns = graphGetAdmissibleVertexNeighbours gs1 newCurrentVertexName gs1
                    (rs2, ys2) = tellTheNeighbours (fromJust ns) newCurrentDistance (reds, ys1)
                in dijkstra' rs2 ys2 gs1 fromName toName newCurrentVertexName newCurrentDistance
