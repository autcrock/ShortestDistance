{-# LANGUAGE DeriveGeneric #-}
module Graph (
    mapToGraph
    , deleteNeighboursByName
    , readStartEndFromString
    , graphGetVertex
    , graphGetVertexNeighbours
    , neighbourHowFarByName
    , graphDeleteVertex
    , graphInsertVertex
    , Graph(..)
    , Vertex(..)
    , Neighbour(..)
) where

import Data.Aeson (eitherDecode, ToJSON, FromJSON)
import Data.Either (Either(..))
import Data.Either.Unwrap (isLeft, fromRight)
import Data.List (sortBy, sortOn, nub, find, deleteBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Map.Ordered as DMO
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy.Encoding as DTE
import GHC.Generics hiding (from, to)
import qualified MapDefinitions as MD

-- Manipulation in Dijkstra
data Neighbour = Neighbour { neighbourName :: Text, howFar :: Double} deriving (Eq, Ord, Show, Generic)
instance ToJSON Neighbour
instance FromJSON Neighbour

type Neighbours = [Neighbour]

data Vertex = Vertex {
        accumulatedDistance :: Double, vertex :: Text, neighbours :: Neighbours
    } deriving ( Eq, Ord, Show, Generic)
instance ToJSON Vertex
instance FromJSON Vertex

newtype Graph = Graph{vertices :: [Vertex]} deriving (Show, Generic)
instance ToJSON Graph
instance FromJSON Graph

listInsertIncreasing :: Ord a => a -> [a] -> [a]
listInsertIncreasing x l = let (lower, greater) = span (< x) l in lower ++ x : greater

listInsertDecreasing :: Ord a => a -> [a] -> [a]
listInsertDecreasing x l = let (greater, lower) = span (> x) l in greater ++ x : lower

sortNeighboursByDistance :: Neighbours -> Neighbours
sortNeighboursByDistance = sortOn howFar

sortVerticesByDistance :: [Vertex] -> [Vertex]
sortVerticesByDistance = sortOn accumulatedDistance

--getVertexNames :: [Connection] -> [Text]
--getVertexNames = nub . Prelude.map from

getVertexNames :: [Vertex] -> [Text]
getVertexNames = nub . Prelude.map vertex

pullVertex :: Text -> [Vertex] -> Double -> Vertex
pullVertex vertexName_in vs accumulatedDistance_in =
    let rawVertices = filter (\x -> vertex x == vertexName_in) vs
        ns = Prelude.map (head. neighbours) rawVertices
    in Vertex { vertex = vertexName_in
              , accumulatedDistance = accumulatedDistance_in
              , neighbours = sortNeighboursByDistance ns }

makeInfinity :: Double
makeInfinity = read "Infinity" :: Double

verticesToGraph :: [Vertex] -> Graph
verticesToGraph vs =
    let infinity = makeInfinity
    in Graph {vertices = sortVerticesByDistance [ pullVertex x vs infinity | x <- getVertexNames vs ]}

insertPlaceInVertices :: MD.Place -> [Vertex] -> [Vertex]
insertPlaceInVertices place vertices =
    vertices ++ placeToVertices place

placeToVertices :: MD.Place -> [Vertex]
placeToVertices p =
    placeToVertices1' (MD.place p) (MD.directConnections p) []

placeToVertices1' :: Text -> [MD.Destination] -> [Vertex] -> [Vertex]
placeToVertices1' _ [] vertices = vertices
placeToVertices1' placeName (destination : destinations) vertices =
    if MD.howFar destination < 0 then error "sd: ERROR: Distances between places must be 0 or positive numbers."
    else 
    let infinity = makeInfinity
    in  vertices
        ++ [Vertex { vertex = placeName,
                    accumulatedDistance = infinity,
                    neighbours = [Neighbour {neighbourName = MD.at destination, howFar = MD.howFar destination}]}]
        ++ [Vertex { vertex = MD.at destination,
                    accumulatedDistance = infinity,
                    neighbours = [Neighbour {neighbourName = placeName, howFar = MD.howFar destination}]}]
        ++ placeToVertices1' placeName destinations vertices

mapToVertices :: MD.Map -> [Vertex]
mapToVertices m =
    let places = MD.map m
    in if null places then []
       else mapToVertices1' places []

mapToVertices1' :: [MD.Place] -> [Vertex] -> [Vertex]
mapToVertices1' [] done = done
mapToVertices1' [place] done  = insertPlaceInVertices place done
mapToVertices1' (place : places) done =
    mapToVertices1' [place] done ++ mapToVertices1' places done

mapToGraph :: MD.Map -> Graph
mapToGraph m =
    let vertices = mapToVertices m
    in verticesToGraph vertices

readStartEndFromString :: Text -> MD.StartEnd
readStartEndFromString candidateStartEnd =
    let eitherStartEnd = eitherDecode $ cs candidateStartEnd :: (Either String MD.StartEnd)
    in case eitherStartEnd of
        Left _ -> error ( "readStartEndFromString: Input [" ++ cs candidateStartEnd ++ "] is not valid.")
        Right r -> r

graphGetVertexNeighbours :: Graph -> Text -> Maybe Neighbours
graphGetVertexNeighbours g v =
    fmap neighbours (graphGetVertex g v)
    -- graphGetVertex g vertex >>= return . neighbours

    -- do
    --     v <- graphGetVertex g vertex
    --     return $ neighbours v

deleteNeighbour :: Neighbours -> Neighbour -> Neighbours
deleteNeighbour ns n =
    deleteBy (\x y -> neighbourName x == neighbourName y) n ns

deleteNeighbourByName :: Neighbours -> Text -> Neighbours
deleteNeighbourByName ns name =
    deleteNeighbour ns Neighbour {neighbourName = name, howFar = 0}

deleteNeighboursByName :: Neighbours -> [Text] -> Neighbours
deleteNeighboursByName [] _ = []
deleteNeighboursByName ns [] = ns
deleteNeighboursByName ns (name:names) =
    deleteNeighboursByName (deleteNeighbourByName ns name) names

neighbourHowFarByName :: Neighbours -> Text -> Double
neighbourHowFarByName ns name =
    if null ns then 0
    else let n = getNeighbour ns name
         in maybe (error "neighbourHowFarByName: Error: Unexpected Nothing returned by getNeighbour.") howFar n

getVertex :: [Vertex] -> Text -> Maybe Vertex
getVertex vs vName = find (\x -> vertex x == vName) vs

getNeighbour :: Neighbours -> Text -> Maybe Neighbour
getNeighbour ns nName = find (\x -> neighbourName x == nName) ns

graphGetVertex :: Graph -> Text -> Maybe Vertex
graphGetVertex pg = getVertex (vertices pg)

deleteVertex :: [Vertex] -> Vertex -> [Vertex]
deleteVertex vs v = deleteBy (\x y -> vertex x == vertex y) v vs

graphDeleteVertex :: Graph -> Vertex -> Graph
graphDeleteVertex pg v =
    let vs = deleteVertex (vertices pg) v
    in Graph { vertices = vs }

graphInsertVertex  :: Graph -> Vertex -> Graph
graphInsertVertex pg v = Graph { vertices = listInsertIncreasing v (vertices pg) }
