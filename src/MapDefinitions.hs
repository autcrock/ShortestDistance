{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}

module MapDefinitions (
      Map(..)
    , Place
    , StartEnd(..)
    , Destination
    , isConnectedTo
    , readMapFromFile
    , readMapFromString
    , place
    , at
    , readMap
    , removeMap
    , saveMap
    , insertPlaces
    , deletePlaces
    , upsertRoad
    , deleteRoad
    , mapToGraph
)

where

import           Control.Exception
import           Control.Monad ()
import           Control.Monad.Extra
import           Data.Aeson (eitherDecode, encode, ToJSON, FromJSON(..))
import qualified Data.ByteString.Lazy as DBSL
import qualified Data.ByteString.Lazy.Char8 as DBSLC8
import           Data.Either.Unwrap (isLeft, isRight, fromLeft, fromRight)
import           Data.List (intersect, deleteBy, isInfixOf, find)
import           Data.Maybe (fromJust, isNothing)
import           Data.Text (Text)
import           GHC.Generics hiding (to)
import           Graph (Vertex(..), Graph(..)
                        , Neighbour(..)
                        , sortVerticesByDistance
                        , associateVertexWithNeighbours
                        , getUniqueVertexNames)
import           System.Directory
import           System.IO.Error

-- Shortest distance query input
data StartEnd = StartEnd {
    start :: !Text,
    end :: !Text
 } deriving (Generic, Show, Eq)
instance ToJSON StartEnd
instance FromJSON StartEnd

-- Shortest distance output
newtype Distance = Distance{distance :: Double}
                     deriving (Generic, Show, Eq)
instance ToJSON Distance
instance FromJSON Distance

-- Map data types
data Destination = Destination {
    at :: !Text,
    howFar :: Double
} deriving (Generic, Show, Eq)

instance ToJSON Destination
instance FromJSON Destination

data Place = Place {
    place :: !Text,
    isConnectedTo :: [Destination]
 } deriving (Generic, Show, Eq)
instance ToJSON Place
instance FromJSON Place

newtype Map = Map{map :: [Place]}
                deriving (Generic, Show, Eq)
instance ToJSON Map
instance FromJSON Map

systemMapFile :: String
systemMapFile = "./SD_CumulativeSystemMapfile.json"

readMapFromFile :: String -> IO (Either String Map)
readMapFromFile inputFile =
    do inputMapAsJSON <- DBSL.readFile inputFile
       let inputMap = eitherDecode inputMapAsJSON :: (Either String Map)
       return inputMap

getPlacesFromFile :: String -> IO [Place]
getPlacesFromFile inputFile = readMapFromFile inputFile >>= getPlacesAST

getPlacesAST :: Either String Map -> IO [Place]
getPlacesAST (Left s) = putStrLn ("sd: getPlacesAST: Error decoding JSON map: " ++ s) >> return []
getPlacesAST (Right m) = putStrLn "sd: getPlacesAST: Extracting places AST: " >> return (MapDefinitions.map m)

saveMap :: Map -> IO ()
saveMap theMap = DBSL.writeFile systemMapFile (encode theMap)

getPlaceNames :: [Place] -> [Text]
getPlaceNames = Prelude.map place 
        
insertPlaces :: Map -> Map -> Map
insertPlaces mapToInsert previousMap =
    if intersection /= []
       then error ("Insertion of an already existing place is not allowed: " ++ show intersection ++ " - Maybe you meant to update instead.")
       else Map {MapDefinitions.map = placesToInsert ++ previousPlaces}
    where
      placesToInsert = MapDefinitions.map mapToInsert
      insertionNames = getPlaceNames placesToInsert
      previousPlaces = MapDefinitions.map previousMap
      previousNames = getPlaceNames previousPlaces
      intersection = insertionNames `intersect` previousNames

deletePlaces :: Map -> Map -> Map
deletePlaces mapToDelete previousMap =
     Map {MapDefinitions.map = filteredPlaces'}
    where
      placesToDelete = MapDefinitions.map mapToDelete
      deletionNames = getPlaceNames placesToDelete
      previousPlaces = MapDefinitions.map previousMap
      filteredPlaces = deletePlaces' deletionNames previousPlaces
      filteredPlaces' = deleteDestinations deletionNames filteredPlaces

deletePlaces' :: [Text] -> [Place] -> [Place]
deletePlaces' [] places = places
deletePlaces' [placeName] places =
    let dummyPlace = Place {place = placeName, isConnectedTo = []}
    in deleteBy (\x y -> place x == place y) dummyPlace places
deletePlaces' (placeName:moreNames) places =
    let nextPlaces = deletePlaces' [placeName] places
    in deletePlaces' moreNames nextPlaces

deleteDestinations :: [Text] -> [Place] -> [Place]
deleteDestinations [] places = places
deleteDestinations [placeName] places = deleteDestinations' placeName places
deleteDestinations (placeName:moreNames) places =
    let nextPlaces = deleteDestinations' placeName places
    in deleteDestinations moreNames nextPlaces

deleteDestinations' :: Text -> [Place] -> [Place]
deleteDestinations' placeName places =
    filter (not . null . isConnectedTo) (Prelude.map (deleteDestinations'' placeName) places)

deleteDestinations'' :: Text -> Place -> Place
deleteDestinations'' destinationName place_in =
    let destinationToDelete = Destination { at = destinationName, howFar = 0}
        ds = deleteBy (\x y -> at x == at y) destinationToDelete $ isConnectedTo place_in
    in Place {place = place place_in, isConnectedTo = ds}

upsertRoad :: Map -> Map -> Map
upsertRoad mapToInsert previousMap
    | null placesToDo || length placesToDo > 1 = error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
    | null ds || length ds > 1 = error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
    | not $ isInfixOf insertionNames previousNames = insertOrReplaceRoad'' newStartOfRoad newDestination previousPs
    | otherwise = insertOrReplaceRoad startOfRoad destination previousPs
    where
        placesToDo = MapDefinitions.map mapToInsert
        insertionNames = getPlaceNames placesToDo
        previousPs = MapDefinitions.map previousMap
        previousNames = getPlaceNames previousPs
        start = head placesToDo
        ds = isConnectedTo start
        startOfRoad = place start
        destination = head ds
        newStartOfRoad = at destination
        newDestination = Destination {at = startOfRoad, howFar = MapDefinitions.howFar destination}

insertOrReplaceRoad :: Text -> Destination -> [Place] -> Map
insertOrReplaceRoad start end previousPlaces =
    Map {MapDefinitions.map = theNewPlace:newPlaces}
    where
      thePlace = fromJust $ find (\x -> start == place x) previousPlaces
      newPlaces = deleteBy (\x y -> place x == place y ) thePlace previousPlaces
      theNewPlace = insertOrReplaceRoad' end thePlace 

insertOrReplaceRoad' :: Destination -> Place -> Place
insertOrReplaceRoad' end thePlace
    | isNothing theEnd = Place { place = place thePlace, isConnectedTo = end:ds }
    | otherwise = Place {place = place thePlace, isConnectedTo = end:newDs}
    where
        ds = isConnectedTo thePlace
        endD = at end
        theEnd = find (\x -> endD == at x) (isConnectedTo thePlace)
        newDs = deleteBy (\x y -> at x == at y ) end ds

insertOrReplaceRoad'' :: Text -> Destination -> [Place] -> Map
insertOrReplaceRoad'' start end previousPlaces
    | isNothing maybeThePlace = error ("sd: Insertion/update of a road requires a known starting location. Tried: " ++ show start ++ " and " ++ show (at end))
    | otherwise = insertOrReplaceRoad start end previousPlaces
    where maybeThePlace = find (\x -> start == place x) previousPlaces
            
    
deleteRoad :: Map -> Map -> Map
deleteRoad mapToDelete previousMap
    | null placesToDo || length placesToDo > 1 = error ("sd: Deletion of only one road at a time is allowed: " ++ show placesToDo)
    | null previousPlaces = error ("sd: There are no places in the database: " ++ show placesToDo)
    | null ds || length ds > 1 = error ("sd: Deletion of only one road at a time is allowed: " ++ show placesToDo)
    | not $ isInfixOf deletionNames previousNames = deleteRoad''' newStartOfRoad newDestination previousPlaces
    | otherwise = deleteRoad' startOfRoad destination previousPlaces
    where
        placesToDo = MapDefinitions.map mapToDelete
        deletionNames = getPlaceNames placesToDo
        previousPlaces = MapDefinitions.map previousMap
        previousNames = getPlaceNames previousPlaces
        start = head placesToDo
        ds = isConnectedTo start
        startOfRoad = place start
        destination = head ds
        newStartOfRoad = at destination
        newDestination = Destination {at = startOfRoad, howFar = MapDefinitions.howFar destination}

deleteRoad' :: Text -> Destination -> [Place] -> Map
deleteRoad' start end previousPlaces =
    let thePlace = fromJust $ find (\x -> start == place x) previousPlaces
        newPlaces = deleteBy (\x y -> place x == place y ) thePlace previousPlaces
        theNewPlace = deleteRoad'' end thePlace 
    in Map {MapDefinitions.map = theNewPlace:newPlaces}

deleteRoad'' :: Destination -> Place -> Place
deleteRoad'' end thePlace
    | isNothing theEnd = thePlace
    | otherwise = Place {place = place thePlace, isConnectedTo = newDs}
    where 
        ds = isConnectedTo thePlace
        endD = at end
        theEnd = find (\x -> endD == at x) ds
        newDs = deleteBy (\x y -> at x == at y ) end ds

deleteRoad''' :: Text -> Destination -> [Place] -> Map
deleteRoad''' start end previousPlaces =
    let maybeThePlace = find (\x -> start == place x) previousPlaces
    in if isNothing maybeThePlace
       then error ("sd: Deletion of a road requires a known starting location. Tried: "
                    ++ show start ++ " and " ++ show (at end))
       else deleteRoad' start end previousPlaces

removeMap :: IO ()
removeMap =
    removeFile systemMapFile `catch` anyErrors
    where anyErrors e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

readMapFromString :: String -> Map
readMapFromString candidateMap =
    let eitherMap = eitherDecode (DBSLC8.pack candidateMap) :: (Either String Map)
    in if isLeft eitherMap 
       then Map { MapDefinitions.map = [] }
       else fromRight eitherMap
    
readMap :: IO Map
readMap =
    do eitherMap <- readMapFromFile systemMapFile
       when (isLeft eitherMap) (putStrLn $ "sd: readMap: " ++ fromLeft eitherMap) >> return Map { MapDefinitions.map = [] }
       return $ fromRight eitherMap

makeInfinity :: Double
makeInfinity = read "Infinity" :: Double

verticesToGraph :: [Vertex] -> Graph
verticesToGraph vs =
    Graph {vertices = sortVerticesByDistance [ associateVertexWithNeighbours x vs makeInfinity | x <- getUniqueVertexNames vs ]}

insertPlaceInVertices :: Place -> [Vertex] -> [Vertex]
insertPlaceInVertices place vertices = vertices ++ placeToVertices place

destinationToVertices :: Text -> Destination -> [Vertex]
destinationToVertices placeName destination =
    [ Vertex { vertex = placeName
            , accumulatedDistance = infinity
            , neighbours = [Neighbour {neighbourName = at destination, howFar = MapDefinitions.howFar destination}]}
    , Vertex { vertex = at destination
                , accumulatedDistance = infinity
                , neighbours = [Neighbour {neighbourName = placeName, howFar = MapDefinitions.howFar destination}]}
    ] where infinity = makeInfinity

-- A place is a name with a list of direct connections, each of which connected pairs is converted to a pair of vertices
placeToVertices :: Place -> [Vertex]
placeToVertices p = placeToVertices' (place p) (isConnectedTo p) []

placeToVertices' :: Text -> [Destination] -> [Vertex] -> [Vertex]
placeToVertices' _ [] vertices = vertices
placeToVertices' placeName (destination : destinations) vertices =
    if MapDefinitions.howFar destination < 0
    then
      error "sd: ERROR: Distances between places must be 0 or positive numbers."
    else 
      vertices
        ++ destinationToVertices placeName destination
        ++ placeToVertices' placeName destinations vertices

mapToVertices :: Map -> [Vertex]
mapToVertices theMap =  foldr insertPlaceInVertices [] (MapDefinitions.map theMap) -- mapToVertices' []  (MapDefinitions.map theMap)

mapToGraph :: Map -> Graph
mapToGraph = verticesToGraph . mapToVertices
