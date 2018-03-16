{-# LANGUAGE DeriveGeneric #-}

module Intermediate (
    mapToGraph
    , readStartEndFromString
    , sortVerticesByDistance
    , graphGetVertex
    , neighbourHowFarByName
    , graphDeleteVertex
    , graphInsertVertex
    , graphGetMinimumYellowByDistance
    , graphGetAdmissibleVertexNeighbours
    , Graph(..)
    , Vertex(..)
    , Neighbour(..)
    , OptionalCompare(..)
    , StartEnd(..)
    , Distance(..)
) where

    import Data.Aeson (eitherDecode, encode, ToJSON, FromJSON)
    import Data.ByteString.Lazy.Char8(pack)
    import Data.Either.Unwrap (isLeft, isRight, fromLeft, fromRight)
    import Data.List (sortBy, nub, find, delete, deleteBy)
    import Data.Maybe (fromJust, fromMaybe, mapMaybe, isNothing)
    import Data.Ord (comparing, min)
    import Data.Text (Text)
    import GHC.Generics hiding (from, to)
    import qualified MapDefinitions as MD
--     (
--        Map, Place, Destination, place, destinations, distance, map, readMap, to
--        ) 
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
    data Distance = Distance {distance:: Double} deriving (Show, Generic, Eq)
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
                 neighbour = to x
                 , howFar = dist x
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
    
    insertPlaceInConnections :: MD.Place -> [Connection] -> [Connection]
    insertPlaceInConnections place connections =
        connections ++ expandPlace place
        
    expandPlace :: MD.Place -> [Connection]
    expandPlace p =
        expandPlace' (MD.place p) (MD.destinations p) []

    expandPlace' :: Text -> [MD.Destination] -> [Connection] -> [Connection]
    expandPlace' placeName [] connections = connections
    expandPlace' placeName [destination] connections =
         connections
         ++ [Connection {
             from = placeName,
             to = MD.to destination,
             dist = MD.distance destination }] 
         ++ [Connection {
            from = MD.to destination,
            to = placeName,
            dist = MD.distance destination }]
    expandPlace' placeName (d : destinations) connections =
        connections
        ++ [Connection {
            from = placeName,
            to = MD.to d,
            dist = MD.distance d }] 
        ++ [Connection {
            from = MD.to d,
            to = placeName,
            dist = MD.distance d }]
        ++ expandPlace' placeName destinations connections

    mapToConnections :: MD.Map -> [Connection]
    mapToConnections map =
        let
            places = MD.map map
        in
            if null places
                then []
                else mapToConnections' places []
    
    mapToConnections' :: [MD.Place] -> [Connection] -> [Connection]
    mapToConnections' [] done = done
    mapToConnections' [place] done  = insertPlaceInConnections place done
    mapToConnections' (place : places) done =
        mapToConnections' [place] done ++ mapToConnections' places done

    mapToGraph :: MD.Map -> Graph
    mapToGraph m =
        let
            connections = mapToConnections m
        in
            connectionsToGraph connections

    readStartEndFromString :: String -> StartEnd
    readStartEndFromString candidateStartEnd =
        let
            eitherStartEnd = eitherDecode (pack candidateStartEnd) :: (Either String StartEnd)
        in
            if isLeft eitherStartEnd 
                then 
                    error ( "readStartEndFromString: Input [" ++ candidateStartEnd ++ "] is not valid.")
                else
                    fromRight eitherStartEnd

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
    
