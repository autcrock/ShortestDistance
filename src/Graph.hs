{-# LANGUAGE DeriveGeneric #-}
module Graph (
    mapToGraph
    , deleteNeighboursByName
    , readStartEndFromString
    , sortVerticesByDistance
    , graphGetVertex
    , graphGetVertexNeighbours
    , neighbourHowFarByName
    , graphDeleteVertex
    , graphInsertVertex
    , Graph(..)
    , Vertex(..)
    , Neighbour(..)
    , OptionalCompare(..)
    , StartEnd(..)
    , Distance(..)
) where

    import Data.Aeson (eitherDecode, ToJSON, FromJSON)
    import Data.ByteString.Lazy.Char8(pack)
    import Data.Either (Either(..))
    import Data.Either.Unwrap (isLeft, fromRight)
    import Data.List (sortBy, sortOn, nub, find, deleteBy)
    import Data.Ord (comparing)
    import Data.Text (Text)
    import GHC.Generics hiding (from, to)
    import qualified MapDefinitions as MD

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

    newtype Graph = Graph{vertices :: [Vertex]} deriving (Show, Generic)
    instance ToJSON Graph
    instance FromJSON Graph

    -- Input and Output
    newtype Distance = Distance{distance :: Double} deriving (Show, Generic, Eq)
    instance ToJSON Distance
    instance FromJSON Distance

    data StartEnd = StartEnd {start:: String, end:: String} deriving (Show, Generic)
    instance ToJSON StartEnd
    instance FromJSON StartEnd

    -- When updating vertex data
    data OptionalCompare = Compare | NoCompare deriving (Eq, Show)

    sortNeighboursByDistance :: [Neighbour] -> [Neighbour]
    sortNeighboursByDistance = sortOn howFar

    sortVerticesByDistance :: [Vertex] -> [Vertex]
    sortVerticesByDistance = sortOn accumulatedDistance

    getVertexNames :: [Connection] -> [Text]
    getVertexNames = nub . Prelude.map from

    pullVertex :: Text -> [Connection] -> Double -> Vertex
    pullVertex vertex_in cs accumulatedDistance_in =
        let
            rawVertices = filter (\x -> from x == vertex_in) cs
            ns = Prelude.map (\x -> Neighbour {
                 neighbour = to x
                 , howFar = dist x
                 }) rawVertices
        in
            Vertex {
                vertex = vertex_in
                , accumulatedDistance = accumulatedDistance_in
                , neighbours = sortNeighboursByDistance ns
                }

    makeInfinity :: Double
    makeInfinity = read "Infinity" :: Double

    connectionsToGraph :: [Connection] -> Graph
    connectionsToGraph cs =
        let
            infinity = makeInfinity
            vs = getVertexNames cs
        in
            Graph {vertices = [ pullVertex x cs infinity | x <- vs ]}
    
    insertPlaceInConnections :: MD.Place -> [Connection] -> [Connection]
    insertPlaceInConnections place connections =
        connections ++ expandPlace place
        
    expandPlace :: MD.Place -> [Connection]
    expandPlace p =
        expandPlace' (MD.place p) (MD.directConnections p) []

    expandPlace' :: Text -> [MD.Destination] -> [Connection] -> [Connection]
    expandPlace' _ [] connections = connections
    expandPlace' placeName [destination] connections =
        if MD.howFar destination < 0
        then error "sd: ERROR: Distances between places must be 0 or positive numbers."
        else
            connections
            ++ [Connection {
                from = placeName,
                to = MD.at destination,
                dist = MD.howFar destination }]
            ++ [Connection {
                from = MD.at destination,
                to = placeName,
                dist = MD.howFar destination }]
    expandPlace' placeName (d : destinations) connections =
        if MD.howFar d < 0
            then error "sd: ERROR: Distances between places must be 0 or positive numbers."
            else
                connections
            ++ [Connection {
                from = placeName,
                to = MD.at d,
                dist = MD.howFar d }]
            ++ [Connection {
                from = MD.at d,
                to = placeName,
                dist = MD.howFar d }]
            ++ expandPlace' placeName destinations connections

    mapToConnections :: MD.Map -> [Connection]
    mapToConnections m =
        let
            places = MD.map m
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
            eitherStartEnd = eitherDecode (Data.ByteString.Lazy.Char8.pack candidateStartEnd) :: (Either String StartEnd)
        in
            if isLeft eitherStartEnd 
                then 
                    error ( "readStartEndFromString: Input [" ++ candidateStartEnd ++ "] is not valid.")
                else
                    fromRight eitherStartEnd

    graphGetVertexNeighbours :: Graph -> Text -> Maybe [Neighbour]
    graphGetVertexNeighbours g v = 
        fmap neighbours (graphGetVertex g v)
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

    getVertex :: [Vertex] -> Text -> Maybe Vertex
    getVertex vs vName =
        find (\x -> vertex x == vName) vs

    getNeighbour :: [Neighbour] -> Text -> Maybe Neighbour
    getNeighbour ns nName =
        -- trace ("getNeighbour calling find on: ns: " ++ show ns ++ ", nName" ++ show nName) $
        find (\x -> neighbour x == nName) ns
        
    graphGetVertex :: Graph -> Text -> Maybe Vertex
    graphGetVertex pg = getVertex (vertices pg)
    
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
    graphInsertVertex pg v =
        Graph { vertices = insertVertex (vertices pg) v }
