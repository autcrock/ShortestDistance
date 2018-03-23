{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module MapDefinitions (
      Map
    , Place
    , Destination
    , destinations
    , distance
    , getMapFromFile
    , getPlacesFromFile
    , readMapFromString
    , MapDefinitions.map
    , place
    , to
    , readMap
    , removeMap
    , saveMap
    , insertPlaces
    , deletePlaces
)

where

    import Control.Exception
    import Control.Monad ()
    import Data.Aeson (eitherDecode, encode, ToJSON, FromJSON(..))
    import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
    import Data.Text (Text)
    import Data.List (intersect, deleteBy)
    import qualified Data.ByteString.Lazy as DBSL
    import qualified Data.ByteString.Lazy.Char8 as DBSLC8
    import GHC.Generics hiding (to)
    import System.Directory
    import System.IO.Error

            
    -- The map defined for storage purposes
    -- This is meant to be saved, edited, read etc and/or passed to the Shortest module for searching

    data Destination = Destination {
        to :: !Text,
        distance :: Double
    } deriving (Generic, Show)

    instance ToJSON Destination
    instance FromJSON Destination

    data Place = Place {
        place :: !Text,
        destinations :: [Destination]
     } deriving (Generic, Show)
    instance ToJSON Place
    instance FromJSON Place

    data Map = Map {
        map :: [Place]
    } deriving (Generic, Show)
    instance ToJSON Map
    instance FromJSON Map
    
    systemMapFile :: String
    systemMapFile = "./SD_CumulativeSystemMapfile.json"

    getMapFromFile :: String -> IO (Either String Map)
    getMapFromFile inputFile =
        do
            inputMapAsJSON <- DBSL.readFile inputFile
            -- DBSLC8.putStrLn inputMapAsJSON
            let inputMap = eitherDecode inputMapAsJSON :: (Either String Map)
            return inputMap

    getPlacesFromFile :: String -> IO [Place]
    getPlacesFromFile inputFile =
        do
            inputMapAST <- getMapFromFile inputFile
            getPlacesAST inputMapAST
                    
    getPlacesAST :: Either String Map -> IO [Place]
    getPlacesAST (Left s) =
        do
            putStrLn $ "sd: getPlacesAST: Error decoding JSON map: " ++ s
            return []

    getPlacesAST (Right m) =
        do
            putStrLn "sd: getPlacesAST: Extracting places AST: "
            let placesAST = MapDefinitions.map m
            return placesAST
                
    saveMap :: Map -> IO ()
    saveMap theMap =
        do
            let encodedMap = encode theMap
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
        in
            if intersection /= []
                then error ("Insertion of an already existing place is not allowed: " ++ show intersection ++ " - Maybe you meant to update instead.")
                else Map {MapDefinitions.map = placesToInsert ++ previousPlaces}

    deletePlaces :: Map -> Map -> Map
    deletePlaces mapToDelete previousMap =
        let placesToDelete = MapDefinitions.map mapToDelete
            deletionNames = getPlaceNames placesToDelete
            previousPlaces = MapDefinitions.map previousMap
            filteredPlaces = deletePlaces' deletionNames previousPlaces
            filteredPlaces' = deleteDestinations deletionNames filteredPlaces
        in
            Map {MapDefinitions.map = filteredPlaces'}

    deletePlaces' :: [Text] -> [Place] -> [Place]
    deletePlaces' [] places = places
    deletePlaces' [placeName] places =
        let dummyPlace = Place {place = placeName, destinations = []}
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
        filter (not . null . destinations)
            (Prelude.map (deleteDestinations'' placeName) places)

    deleteDestinations'' :: Text -> Place -> Place
    deleteDestinations'' destinationName place_in =
        let destinationToDelete = Destination { to = destinationName, distance = 0}
            ds = deleteBy (\x y -> to x == to y) destinationToDelete $ destinations place_in
        in Place {place = place place_in, destinations = ds}

    removeMap :: IO ()
    removeMap =
        removeFile systemMapFile `catch` anyErrors
        where anyErrors e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e

    readMapFromString :: String -> Map
    readMapFromString candidateMap =
        let
            eitherMap = eitherDecode (DBSLC8.pack candidateMap) :: (Either String Map)
        in
            if isLeft eitherMap 
                then 
                    Map { MapDefinitions.map = [] }
                else
                    fromRight eitherMap
        
    readMap :: IO Map
    readMap =
        do
        eitherMap <- getMapFromFile systemMapFile
        if isLeft eitherMap 
        then 
            (do
            putStrLn $ "sd: readMap: " ++ fromLeft eitherMap
            return Map { MapDefinitions.map = [] })
        else return (fromRight eitherMap)
