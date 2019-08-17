{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings, DisambiguateRecordFields #-}

module MapDefinitions (
      Map(..)
    , Place
    , StartEnd(..)
    , Destination
    , directConnections
    , howFar
    , getMapFromFile
    , getPlacesFromFile
    , readMapFromString
    , place
    , at
    , readMap
    , removeMap
    , saveMap
    , insertPlaces
    , deletePlaces
    , insertOrModifyRoad
    , deleteRoad
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
        directConnections :: [Destination]
     } deriving (Generic, Show, Eq)
    instance ToJSON Place
    instance FromJSON Place

    newtype Map = Map{map :: [Place]}
                    deriving (Generic, Show, Eq)
    instance ToJSON Map
    instance FromJSON Map
    
    systemMapFile :: String
    systemMapFile = "./SD_CumulativeSystemMapfile.json"

    getMapFromFile :: String -> IO (Either String Map)
    getMapFromFile inputFile =
        do inputMapAsJSON <- DBSL.readFile inputFile
           let inputMap = eitherDecode inputMapAsJSON :: (Either String Map)
           return inputMap

    getPlacesFromFile :: String -> IO [Place]
    getPlacesFromFile inputFile =
        do inputMapAST <- getMapFromFile inputFile
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
        let dummyPlace = Place {place = placeName, directConnections = []}
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
        filter (not . null . directConnections)
            (Prelude.map (deleteDestinations'' placeName) places)

    deleteDestinations'' :: Text -> Place -> Place
    deleteDestinations'' destinationName place_in =
        let destinationToDelete = Destination { at = destinationName, howFar = 0}
            ds = deleteBy (\x y -> at x == at y) destinationToDelete $ directConnections place_in
        in Place {place = place place_in, directConnections = ds}

    insertOrModifyRoad :: Map -> Map -> Map
    insertOrModifyRoad mapToInsert previousMap =
        let placesToDo = MapDefinitions.map mapToInsert
        in if null placesToDo || length placesToDo > 1
           then error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
           else 
             let insertionNames = getPlaceNames placesToDo
                 previousPs = MapDefinitions.map previousMap
                 previousNames = getPlaceNames previousPs
                 start = head placesToDo
                 ds = directConnections start
             in if null ds || length ds > 1
                then error ("sd: Insertion/modification of only one road at a time is allowed: " ++ show placesToDo)
                else let startOfRoad = place start
                         destination = head ds
                     in if not $ isInfixOf insertionNames previousNames
                        then let newStartOfRoad = at destination
                                 newDestination = Destination {at = startOfRoad, howFar = howFar destination}
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
        let ds = directConnections thePlace
            endD = at end
            theEnd = find (\x -> endD == at x) (directConnections thePlace)
        in if isNothing theEnd
           then Place { place = place thePlace, directConnections = end:ds }
           else let newDs = deleteBy (\x y -> at x == at y ) end ds
                in Place {place = place thePlace, directConnections = end:newDs}

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
                            ds = directConnections start
                        in if null ds || length ds > 1
                           then error ("sd: Deletion of only one road at a time is allowed: " ++ show placesToDo)
                           else let startOfRoad = place start
                                    destination = head ds
                                in if not $ isInfixOf deletionNames previousNames
                                   then let newStartOfRoad = at destination
                                            newDestination = Destination {at = startOfRoad, howFar = howFar destination}
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
        let ds = directConnections thePlace
            endD = at end
            theEnd = find (\x -> endD == at x) ds
        in if isNothing theEnd
           then thePlace
           else let newDs = deleteBy (\x y -> at x == at y ) end ds
                in Place {place = place thePlace, directConnections = newDs}


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
        do eitherMap <- getMapFromFile systemMapFile
           if isLeft eitherMap 
           then (do putStrLn $ "sd: readMap: " ++ fromLeft eitherMap
                    return Map { MapDefinitions.map = [] })
           else return (fromRight eitherMap)
