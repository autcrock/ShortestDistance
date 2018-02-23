{-# LANGUAGE DeriveGeneric #-}

module MapDefinitions (
      Map
    , Destination
    , getMapFromFile
    , getPlacesFromFile
    , addNodeToMap
    , deleteNodeFromMap
    , saveMap
    , removeMap
)

where

    import Control.Exception
    import Data.Aeson
    import Data.Text
    import qualified Data.ByteString.Lazy as DBSL
    import qualified Data.ByteString.Lazy.Char8 as DBSLC8
    import GHC.Generics
    import Prelude hiding (catch)
    import System.Directory
    import System.IO.Error hiding (catch)

            
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

    -- data Map = Map {
    --     map :: [Object]
    -- } deriving (Generic, Show)

    -- newtype From = From String deriving (Generic, Show)
    -- instance ToJSON From
    -- instance FromJSON From

    -- newtype To = To String deriving (Generic, Show)
    -- instance ToJSON To
    -- instance FromJSON To

    -- newtype Dest = Dest Double deriving (Generic, Show)
    -- instance ToJSON Dest
    -- instance FromJSON Dest

    
    -- data Map = Map {
    --     map :: [(From, [(To, Dest)])]
    -- }  deriving (Generic, Show)
    -- instance ToJSON Map
    -- instance FromJSON Map

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
            DBSLC8.putStrLn inputMapAsJSON
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

    removeMap :: IO ()
    removeMap =
        removeFile systemMapFile `catch` anyErrors
        where anyErrors e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e

    -- unimplemented spacefillers below
    addNodeToMap :: Map -> String -> Map
    addNodeToMap map node = map

    deleteNodeFromMap :: Map -> String -> Map
    deleteNodeFromMap map node = map
