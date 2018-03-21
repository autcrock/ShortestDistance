{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module MapDefinitions (
      Map
    , Place
    , Destination
    , addNodeToMap
    , deleteNodeFromMap
    , destinations
    , distance
    , getMapFromFile
    , getPlacesFromFile
    , map
    , place
    , to
    , readMap
    , removeMap
    , saveMap
)

where

    import Control.Exception
    import Control.Monad ()
    import Data.Aeson (eitherDecode, encode, ToJSON, FromJSON)
    import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
    import Data.Text (Text)
    import qualified Data.ByteString.Lazy as DBSL
    import qualified Data.ByteString.Lazy.Char8 as DBSLC8
    import GHC.Generics hiding (to)
    import Prelude hiding (map)
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
--            DBSLC8.putStrLn inputMapAsJSON
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
            let placesAST = map m
            return placesAST
                
    saveMap :: Map -> IO ()
    saveMap theMap =
        do
            let encodedMap = encode theMap
            DBSL.writeFile systemMapFile encodedMap

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
                    Map { map = [] }
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
            return Map { map = [] })
        else return (fromRight eitherMap)

    -- unimplemented spacefillers below

    addNodeToMap :: Map -> String -> Map
    addNodeToMap m node = m

    deleteNodeFromMap :: Map -> String -> Map
    deleteNodeFromMap m node = m

