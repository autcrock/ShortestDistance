module GraphOperations (
    deleteNeighboursByName
    , graphGetVertex
    , graphGetVertexNeighbours
    , neighbourHowFarByName
    , graphDeleteVertex
    , graphInsertVertex
    , sortVerticesByDistance
    , associateVertexWithNeighbours
    , getUniqueVertexNames
) where

import           Data.List (sortBy, sortOn, nub, find, deleteBy)
import           Data.Text (Text)
import Neighbour ( Neighbours, Neighbour(..) )
import Vertex ( Vertices, Vertex(..) )
import Graph ( Graph(..) )

-- Manipulation in Dijkstra
listInsertIncreasing :: Ord a => a -> [a] -> [a]
listInsertIncreasing x l = let (lower, greater) = span (< x) l in lower ++ x : greater

listInsertDecreasing :: Ord a => a -> [a] -> [a]
listInsertDecreasing x l = let (greater, lower) = span (> x) l in greater ++ x : lower

sortNeighboursByDistance :: Neighbours -> Neighbours
sortNeighboursByDistance = sortOn howFar

sortVerticesByDistance :: Vertices -> Vertices
sortVerticesByDistance = sortOn accumulatedDistance

getUniqueVertexNames :: Vertices -> [Text]
getUniqueVertexNames = nub . Prelude.map vertex

-- associateVertexWithNeighbours is only used during conversion of a map to a fully developed
-- Graph vertex.  It pulls all direct references to the paticular vertex in as neighbours for the instant vertex.
-- This expresses the two way relationship between neighbours as places in the Graph  
associateVertexWithNeighbours :: Text -> Vertices -> Double -> Vertex
associateVertexWithNeighbours vertexName_in vertices accumulatedDistance_in =
    Vertex { vertex = vertexName_in
            , accumulatedDistance = accumulatedDistance_in
            , neighbours = sortNeighboursByDistance newNeighbours }
    where
        rawVertices = filter (\x -> vertex x == vertexName_in) vertices
        newNeighbours = Prelude.map (head . neighbours) rawVertices

graphGetVertexNeighbours :: Graph -> Text -> Maybe Neighbours
graphGetVertexNeighbours g v = fmap neighbours (graphGetVertex g v)

-- Two alternative ways to express graphGetVertexNeighbours
    -- graphGetVertex g vertex >>= return . neighbours

    -- do
    --     v <- graphGetVertex g vertex
    --     return $ neighbours v

deleteNeighbour :: Neighbour -> Neighbours -> Neighbours
deleteNeighbour = deleteBy (\x y -> neighbourName x == neighbourName y)

deleteNeighbourByName :: Text -> Neighbours -> Neighbours
deleteNeighbourByName name = deleteNeighbour Neighbour {neighbourName = name, howFar = 0}

deleteNeighboursByName :: [Text] -> Neighbours -> Neighbours
deleteNeighboursByName _ [] = []
deleteNeighboursByName [] neighbours = neighbours
deleteNeighboursByName (name:names) neighbours = deleteNeighboursByName names (deleteNeighbourByName name neighbours)

neighbourHowFarByName :: Neighbours -> Text -> Double
neighbourHowFarByName neighbours name =
    if null neighbours then 0
    else maybe (error "neighbourHowFarByName: Error: Unexpected Nothing returned by getNeighbour.") howFar neighbour
    where neighbour = getNeighbour neighbours name

getVertex :: Vertices -> Text -> Maybe Vertex
getVertex vertices vName = find (\x -> vertex x == vName) vertices

getNeighbour :: Neighbours -> Text -> Maybe Neighbour
getNeighbour neighbours nName = find (\x -> neighbourName x == nName) neighbours

graphGetVertex :: Graph -> Text -> Maybe Vertex
graphGetVertex pg = getVertex (vertices pg)

deleteVertex :: Vertices -> Vertex -> Vertices
deleteVertex vertices v = deleteBy (\x y -> vertex x == vertex y) v vertices

graphDeleteVertex :: Graph -> Vertex -> Graph
graphDeleteVertex pg vertex =
    let newVertices = deleteVertex (vertices pg) vertex
    in Graph { vertices = newVertices }

graphInsertVertex  :: Graph -> Vertex -> Graph
graphInsertVertex pg v = Graph { vertices = listInsertIncreasing v (vertices pg) }
