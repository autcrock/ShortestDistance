{-# LANGUAGE DeriveGeneric #-}
module Graph (
    deleteNeighboursByName
    , graphGetVertex
    , graphGetVertexNeighbours
    , neighbourHowFarByName
    , graphDeleteVertex
    , graphInsertVertex
    , sortVerticesByDistance
    , associateVertexWithNeighbours
    , getUniqueVertexNames
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
import qualified Data.Text.Lazy.Encoding as DTE
import GHC.Generics hiding (from, to)

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

getUniqueVertexNames :: [Vertex] -> [Text]
getUniqueVertexNames = nub . Prelude.map vertex

associateVertexWithNeighbours :: Text -> [Vertex] -> Double -> Vertex
associateVertexWithNeighbours vertexName_in vs accumulatedDistance_in =
    let rawVertices = filter (\x -> vertex x == vertexName_in) vs
        ns = Prelude.map (head . neighbours) rawVertices
    in Vertex { vertex = vertexName_in
              , accumulatedDistance = accumulatedDistance_in
              , neighbours = sortNeighboursByDistance ns }

graphGetVertexNeighbours :: Graph -> Text -> Maybe Neighbours
graphGetVertexNeighbours g v = fmap neighbours (graphGetVertex g v)

-- Two alternative ways to express graphGetVertexNeighbours
    -- graphGetVertex g vertex >>= return . neighbours

    -- do
    --     v <- graphGetVertex g vertex
    --     return $ neighbours v

deleteNeighbour :: Neighbours -> Neighbour -> Neighbours
deleteNeighbour ns n = deleteBy (\x y -> neighbourName x == neighbourName y) n ns

deleteNeighbourByName :: Neighbours -> Text -> Neighbours
deleteNeighbourByName ns name = deleteNeighbour ns Neighbour {neighbourName = name, howFar = 0}

deleteNeighboursByName :: Neighbours -> [Text] -> Neighbours
deleteNeighboursByName [] _ = []
deleteNeighboursByName ns [] = ns
deleteNeighboursByName ns (name:names) = deleteNeighboursByName (deleteNeighbourByName ns name) names

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
