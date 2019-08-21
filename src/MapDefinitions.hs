{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings, DisambiguateRecordFields #-}

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

import Control.Exception
import Control.Monad ()
import Data.Aeson (eitherDecode, encode, ToJSON, FromJSON(..))
import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
import Data.Text (Text)
import Data.Maybe (fromJust, isNothing)
import Data.List (intersect, deleteBy, isInfixOf, find)
import qualified Data.ByteString.Lazy as DBSL
import qualified Data.ByteString.Lazy.Char8 as DBSLC8
import GHC.Generics hiding (to)
import System.Directory
import System.IO.Error
import Graph (Vertex(..)
            , Graph(..)
            , Neighbour(..)
            , sortVerticesByDistance
            , associateVertexWithNeighbours
            , getUniqueVertexNames)

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
getPlacesFromFile inputFile =
    do inputMapAST <- readMapFromFile inputFile
       getPlacesAST inputMapAST
                
getPlacesAST :: Either String Map -> IO [Place]
getPlacesAST (Left s) =
    do putStrLn $ "sd: getPlacesAST: Error decoding JSON map: " ++ s
       return []

getPlacesAST (Right m) =
    do putStrLn "sd: getPlacesAST: Extracting places AST: "
       let placesAST = MapDefinitions.map m
       return placesAST
            
saveMap :: Map -> IO ()
saveMap theMap =
    do let encodedMap = encode theMap
       DBSL.writeFile systemMapFile encodedMap

getPlaceNames :: [Place] -> [Text]
getPlaceNames = Prelude.map place 
        
insertPlaces :: Map -> Map -> Map
insertPlaces mapToInsert previousMap =
    let placesToInsert = MapDefinitions.map mapToInsert
        insertionNames = getPlaceNames placesToInsert
        previousPlaces = MapDefinitions.map previousMap
        previousNames = getPlaceNames previousPlaces
        intersection = intersect insertionNames previousNames
    in if intersection /= []
       then error ("Insertion of an already existing place is not allowed: " ++ show intersection ++ " - Maybe you meant to update instead.")
       else Map {MapDefinitions.map = placesToInsert ++ previousPlaces}

deletePlaces :: Map -> Map -> Map
deletePlaces mapToDelete previousMap =
    let placesToDelete = MapDefinitions.map mapToDelete
        deletionNames = getPlaceNames placesToDelete
        previousPlaces = MapDefinitions.map previousMap
        filteredPlaces = deletePlaces' deletionNames previousPlaces
        filteredPlaces' = deleteDestinations deletionNames filteredPlaces
    in Map {MapDefinitions.map = filteredPlaces'}

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
    filter (not . null . isConnectedTo)
        (Prelude.map (deleteDestinations'' placeName) places)

deleteDestinations'' :: Text -> Place -> Place
deleteDestinations'' destinationName place_in =
    let destinationToDelete = Destination { at = destinationName, howFar = 0}
        ds = deleteBy (\x y -> at x == at y) destinationToDelete $ isConnectedTo place_in
    in Place {place = place place_in, isConnectedTo = ds}

upsertRoad :: Map -> Map -> Map
upsertRoad mapToInsert previousMap =
    let placesToDo = MapDefinitions.map mapToInsert
    in if null placesToDo || length placesToDo > 1
       then error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
       else 
         let insertionNames = getPlaceNames placesToDo
             previousPs = MapDefinitions.map previousMap
             previousNames = getPlaceNames previousPs
             start = head placesToDo
             ds = isConnectedTo start
         in if null ds || length ds > 1
            then error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
            else let startOfRoad = place start
                     destination = head ds
                 in if not $ isInfixOf insertionNames previousNames
                    then let newStartOfRoad = at destination
                             newDestination = Destination {at = startOfRoad, howFar = MapDefinitions.howFar destination}
                         in insertOrReplaceRoad'' newStartOfRoad newDestination previousPs
                    else insertOrReplaceRoad startOfRoad destination previousPs

insertOrReplaceRoad :: Text -> Destination -> [Place] -> Map
insertOrReplaceRoad start end previousPlaces =
    let thePlace = fromJust $ find (\x -> start == place x) previousPlaces
        newPlaces = deleteBy (\x y -> place x == place y ) thePlace previousPlaces
        theNewPlace = insertOrReplaceRoad' end thePlace 
    in Map {MapDefinitions.map = theNewPlace:newPlaces}

insertOrReplaceRoad' :: Destination -> Place -> Place
insertOrReplaceRoad' end thePlace =
    let ds = isConnectedTo thePlace
        endD = at end
        theEnd = find (\x -> endD == at x) (isConnectedTo thePlace)
    in if isNothing theEnd
       then Place { place = place thePlace, isConnectedTo = end:ds }
       else let newDs = deleteBy (\x y -> at x == at y ) end ds
            in Place {place = place thePlace, isConnectedTo = end:newDs}

insertOrReplaceRoad'' :: Text -> Destination -> [Place] -> Map
insertOrReplaceRoad'' start end previousPlaces =
    let maybeThePlace = find (\x -> start == place x) previousPlaces
    in if isNothing maybeThePlace
       then error ("sd: Insertion/update of a road requires a known starting location. Tried: "
                    ++ show start ++ " and " ++ show (at end))
       else insertOrReplaceRoad start end previousPlaces
            
    
deleteRoad :: Map -> Map -> Map
deleteRoad mapToDelete previousMap =
    let placesToDo = MapDefinitions.map mapToDelete
    in if null placesToDo || length placesToDo > 1
       then error ("sd: Deletion of only one road at a time is allowed: " ++ show placesToDo)
       else let deletionNames = getPlaceNames placesToDo
                previousPlaces = MapDefinitions.map previousMap
            in if null previousPlaces
               then error ("sd: There are no places in the database: " ++ show placesToDo)
               else let previousNames = getPlaceNames previousPlaces
                        start = head placesToDo
                        ds = isConnectedTo start
                    in if null ds || length ds > 1
                       then error ("sd: Deletion of only one road at a time is allowed: " ++ show placesToDo)
                       else let startOfRoad = place start
                                destination = head ds
                            in if not $ isInfixOf deletionNames previousNames
                               then let newStartOfRoad = at destination
                                        newDestination = Destination {at = startOfRoad, howFar = MapDefinitions.howFar destination}
                                    in deleteRoad''' newStartOfRoad newDestination previousPlaces
                                    else deleteRoad' startOfRoad destination previousPlaces

deleteRoad' :: Text -> Destination -> [Place] -> Map
deleteRoad' start end previousPlaces =
    let thePlace = fromJust $ find (\x -> start == place x) previousPlaces
        newPlaces = deleteBy (\x y -> place x == place y ) thePlace previousPlaces
        theNewPlace = deleteRoad'' end thePlace 
    in Map {MapDefinitions.map = theNewPlace:newPlaces}

deleteRoad'' :: Destination -> Place -> Place
deleteRoad'' end thePlace =
    let ds = isConnectedTo thePlace
        endD = at end
        theEnd = find (\x -> endD == at x) ds
    in if isNothing theEnd
       then thePlace
       else let newDs = deleteBy (\x y -> at x == at y ) end ds
            in Place {place = place thePlace, isConnectedTo = newDs}


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
       if isLeft eitherMap 
       then (do putStrLn $ "sd: readMap: " ++ fromLeft eitherMap
                return Map { MapDefinitions.map = [] })
       else return (fromRight eitherMap)

makeInfinity :: Double
makeInfinity = read "Infinity" :: Double

verticesToGraph :: [Vertex] -> Graph
verticesToGraph vs =
    let infinity = makeInfinity
    in Graph {vertices = sortVerticesByDistance [ associateVertexWithNeighbours x vs infinity | x <- getUniqueVertexNames vs ]}

insertPlaceInVertices :: Place -> [Vertex] -> [Vertex]
insertPlaceInVertices place vertices =
    vertices ++ placeToVertices place

-- A place is a name with a list of direct connections, each of which connected pairs is converted to a pair of vertices
placeToVertices :: Place -> [Vertex]
placeToVertices p =
    placeToVertices1' (place p) (isConnectedTo p) []

placeToVertices1' :: Text -> [Destination] -> [Vertex] -> [Vertex]
placeToVertices1' _ [] vertices = vertices
placeToVertices1' placeName (destination : destinations) vertices =
    if MapDefinitions.howFar destination < 0 then error "sd: ERROR: Distances between places must be 0 or positive numbers."
    else 
    let infinity = makeInfinity
    in  vertices
        ++ [Vertex { vertex = placeName,
                    accumulatedDistance = infinity,
                    neighbours = [Neighbour {neighbourName = at destination, howFar = MapDefinitions.howFar destination}]}]
        ++ [Vertex { vertex = at destination,
                    accumulatedDistance = infinity,
                    neighbours = [Neighbour {neighbourName = placeName, howFar = MapDefinitions.howFar destination}]}]
        ++ placeToVertices1' placeName destinations vertices

mapToVertices :: Map -> [Vertex]
mapToVertices theMap =
    let places = MapDefinitions.map theMap
    in if null places then []
       else mapToVertices1' places []

mapToVertices1' :: [Place] -> [Vertex] -> [Vertex]
mapToVertices1' [] done = done
mapToVertices1' [place] done  = insertPlaceInVertices place done
mapToVertices1' (place : places) done =
    mapToVertices1' [place] done ++ mapToVertices1' places done

mapToGraph :: Map -> Graph
mapToGraph = verticesToGraph . mapToVertices

