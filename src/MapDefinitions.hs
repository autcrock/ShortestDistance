{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE OverloadedStrings        #-}

module MapDefinitions (
    isConnectedTo
    , readMapFromFile
    , readMapFromString
    , readMap
    , removeMap
    , saveMap
    , insertPlaces
    , deletePlaces
    , upsertRoad
    , deleteRoad
    , mapToGraph
) where

import           Control.Exception
import           Control.Monad.Extra
import           Data.Aeson (eitherDecode, encode, ToJSON, FromJSON(..))
import qualified Data.ByteString.Lazy as DBSL
import qualified Data.ByteString.Lazy.Char8 as DBSLC8
import           Data.Either.Unwrap (isLeft, isRight, fromLeft, fromRight)
import           Data.List (intersect, deleteBy, isInfixOf, find)
import           Data.Maybe (fromJust, isNothing)
import           Data.Text (Text)
import           GHC.Generics hiding (to)
import           System.Directory
import           System.IO.Error
import           Graph
import           Map
import           Destination
import           Place
import           StartEnd
import           Vertex
import           Neighbour
import           GraphOperations


systemMapFile :: String
systemMapFile = "./SD_CumulativeSystemMapfile.json"

readMapFromFile :: String -> IO (Either String Map)
readMapFromFile inputFile =
    do inputMapAsJSON <- DBSL.readFile inputFile
       let inputMap = eitherDecode inputMapAsJSON :: (Either String Map)
       return inputMap

getPlacesFromFile :: String -> IO Places
getPlacesFromFile inputFile = readMapFromFile inputFile >>= getPlacesAST

getPlacesAST :: Either String Map -> IO Places
getPlacesAST (Left s) = putStrLn ("sd: getPlacesAST: Error decoding JSON map: " ++ s) >> return []
getPlacesAST (Right m) = putStrLn "sd: getPlacesAST: Extracting places AST: " >> return (Map.map m)

saveMap :: Map -> IO ()
saveMap theMap = DBSL.writeFile systemMapFile (encode theMap)

getPlaceNames :: Places -> [Text]
getPlaceNames = Prelude.map place 
        
insertPlaces :: Map -> Map -> Map
insertPlaces mapToInsert previousMap =
    if intersection /= []
       then error ("Insertion of an already existing place is not allowed: " ++ show intersection ++ " - Maybe you meant to update instead.")
       else Map {map = placesToInsert ++ previousPlaces}
    where
      placesToInsert = Map.map mapToInsert
      insertionNames = getPlaceNames placesToInsert
      previousPlaces = Map.map previousMap
      previousNames = getPlaceNames previousPlaces
      intersection = insertionNames `intersect` previousNames

deletePlaces :: Map -> Map -> Map
deletePlaces mapToDelete previousMap =
     Map {map = filteredPlaces'}
    where
      placesToDelete = Map.map mapToDelete
      deletionNames = getPlaceNames placesToDelete
      previousPlaces = Map.map previousMap
      filteredPlaces = deletePlaces' deletionNames previousPlaces
      filteredPlaces' = deleteDestinations deletionNames filteredPlaces

deletePlaces' :: [Text] -> Places -> Places
deletePlaces' [] places = places
deletePlaces' [placeName] places =
    let dummyPlace = Place {place = placeName, isConnectedTo = []}
    in deleteBy (\x y -> place x == place y) dummyPlace places
deletePlaces' (placeName:moreNames) places =
    let nextPlaces = deletePlaces' [placeName] places
    in deletePlaces' moreNames nextPlaces

deleteDestinations :: [Text] -> Places -> Places
deleteDestinations [] places = places
deleteDestinations [placeName] places = deleteDestinations' placeName places
deleteDestinations (placeName:moreNames) places =
    let nextPlaces = deleteDestinations' placeName places
    in deleteDestinations moreNames nextPlaces

deleteDestinations' :: Text -> Places -> Places
deleteDestinations' placeName places =
    filter (not . null . isConnectedTo) (Prelude.map (deleteDestinations'' placeName) places)

deleteDestinations'' :: Text -> Place -> Place
deleteDestinations'' destinationName placeIn =
    Place { place = place placeIn,
            isConnectedTo = deleteBy (\x y -> at x == at y)
                                     (Destination { at = destinationName, howFar = 0})
                                     (isConnectedTo placeIn) }

upsertRoad :: Map -> Map -> Map
upsertRoad mapToInsert previousMap
    | null placesToDo || length placesToDo > 1 = error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
    | null ds || length ds > 1 = error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
    | not $ isInfixOf insertionNames previousNames = insertOrReplaceRoad'' newStartOfRoad newDestination previousPs
    | otherwise = insertOrReplaceRoad startOfRoad destination previousPs
    where
        placesToDo = Map.map mapToInsert
        insertionNames = getPlaceNames placesToDo
        previousPs = Map.map previousMap
        previousNames = getPlaceNames previousPs
        start = head placesToDo
        ds = isConnectedTo start
        startOfRoad = place start
        destination = head ds
        newStartOfRoad = at destination
        newDestination = Destination {at = startOfRoad, howFar = Destination.howFar destination}

insertOrReplaceRoad :: Text -> Destination -> Places -> Map
insertOrReplaceRoad start end previousPlaces =
    Map {map = theNewPlace:newPlaces}
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

insertOrReplaceRoad'' :: Text -> Destination -> Places -> Map
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
        placesToDo = Map.map mapToDelete
        deletionNames = getPlaceNames placesToDo
        previousPlaces = Map.map previousMap
        previousNames = getPlaceNames previousPlaces
        start = head placesToDo
        ds = isConnectedTo start
        startOfRoad = place start
        destination = head ds
        newStartOfRoad = at destination
        newDestination = Destination {at = startOfRoad, howFar = Destination.howFar destination}

deleteRoad' :: Text -> Destination -> Places -> Map
deleteRoad' start end previousPlaces =
    let thePlace = fromJust $ find (\x -> start == place x) previousPlaces
        newPlaces = deleteBy (\x y -> place x == place y ) thePlace previousPlaces
        theNewPlace = deleteRoad'' end thePlace 
    in Map {map = theNewPlace:newPlaces}

deleteRoad'' :: Destination -> Place -> Place
deleteRoad'' end thePlace
    | isNothing theEnd = thePlace
    | otherwise = Place {place = place thePlace, isConnectedTo = newDs}
    where 
        ds = isConnectedTo thePlace
        endD = at end
        theEnd = find (\x -> endD == at x) ds
        newDs = deleteBy (\x y -> at x == at y ) end ds

deleteRoad''' :: Text -> Destination -> Places -> Map
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
       then Map { map = [] }
       else fromRight eitherMap
    
readMap :: IO Map
readMap =
    do eitherMap <- readMapFromFile systemMapFile
       when (isLeft eitherMap) (putStrLn $ "sd: readMap: " ++ fromLeft eitherMap) >> return Map { map = [] }
       return $ fromRight eitherMap

makeInfinity :: Double
makeInfinity = read "Infinity" :: Double

verticesToGraph :: Vertices -> Graph
verticesToGraph vs =
    Graph {vertices = sortVerticesByDistance [ associateVertexWithNeighbours x vs makeInfinity | x <- getUniqueVertexNames vs ]}

insertPlaceInVertices :: Place -> Vertices -> Vertices
insertPlaceInVertices place vertices = vertices ++ placeToVertices place

destinationToVertices :: Text -> Destination -> Vertices
destinationToVertices placeName destination =
    [ Vertex { vertex = placeName
            , accumulatedDistance = infinity
            , neighbours = [Neighbour {neighbourName = at destination, howFar = Destination.howFar destination}]}
    , Vertex { vertex = at destination
                , accumulatedDistance = infinity
                , neighbours = [Neighbour {neighbourName = placeName, howFar = Destination.howFar destination}]}
    ] where infinity = makeInfinity

-- A place is a name with a list of direct connections, each of which connected pairs is converted to a pair of vertices
placeToVertices :: Place -> Vertices
placeToVertices p = placeToVertices' (place p) (isConnectedTo p) []

placeToVertices' :: Text -> Destinations -> Vertices -> Vertices
placeToVertices' _ [] vertices = vertices
placeToVertices' placeName (destination : destinations) vertices =
    if Destination.howFar destination < 0
    then
      error "sd: ERROR: Distances between places must be 0 or positive numbers."
    else
      vertices
        ++ destinationToVertices placeName destination
        ++ placeToVertices' placeName destinations vertices

mapToVertices :: Map -> Vertices
mapToVertices theMap =  foldr insertPlaceInVertices [] (Map.map theMap) -- mapToVertices' []  (Map.map theMap)

mapToGraph :: Map -> Graph
mapToGraph = verticesToGraph . mapToVertices
