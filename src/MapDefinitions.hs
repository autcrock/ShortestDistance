{-# LANGUAGE DeriveGeneric #-}

module MapDefinitions (
      Map
    , Destination
    , ToPlaceName
    , FromPlaceName
    , Distance
    , getMapFromFile
    , addNodeToMap
    , deleteNodeFromMap
    , saveMap
    , removeMap
)

where

    import GHC.Generics
    import Data.Aeson
    import qualified Data.ByteString.Lazy as DBSL
    import Prelude hiding (catch)
    import System.IO.Error hiding (catch)
    import System.Directory
    import Control.Exception
            
    -- The map defined for storage purposes
    -- This is meant to be saved, edited, read etc and/or passed to the Shortest module for searching

    newtype FromPlaceName = FromPlaceName String deriving (Generic, Show)
    instance ToJSON FromPlaceName
    instance FromJSON FromPlaceName

    newtype ToPlaceName = ToPlaceName String deriving (Generic, Show)
    instance ToJSON ToPlaceName
    instance FromJSON ToPlaceName

    newtype Distance = Distance Double deriving (Generic, Show)
    instance ToJSON Distance
    instance FromJSON Distance
    
    data Destination = Destination ToPlaceName Distance deriving (Generic, Show)
    instance ToJSON Destination
    instance FromJSON Destination

    data Place = Place FromPlaceName [Destination] deriving (Generic, Show)
    instance ToJSON Place
    instance FromJSON Place

    data Map = Map [Place] deriving (Generic, Show)
    instance ToJSON Map
    instance FromJSON Map
        
    systemMapFile :: String
    systemMapFile = "./SD_CumulativeSystemMapfile.json"

    getMapFromFile :: String -> IO (Either String Map)
    getMapFromFile inputFile =
        do
            inputMapAsJSON <- DBSL.readFile inputFile
            let inputMap = eitherDecode inputMapAsJSON :: (Either String Map)
            return inputMap

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
