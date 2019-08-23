module MapOperations
    (
        aPlace
        , clear
        , xPlace
        , initialise
        , initialiseJSON
        , aRoad
        , shortest
        , xRoad
    ) where

import Data.Aeson (encode)
import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
import Data.Either.Combinators (mapBoth)
import Data.String.Conversions (cs)
import Control.Applicative

import MapDefinitions
    ( Map
    , readMapFromFile
    , readMapFromString
    , readMap
    , saveMap
    , removeMap
    , insertPlaces
    , deletePlaces
    , upsertRoad
    , deleteRoad
    )

import Shortest (dijkstra)
import Control.Monad

mapOperation :: String -> (Map -> Map -> Map) -> String -> IO()
mapOperation message operation couldBeJSON =
  putStrLn (message ++ " [" ++ couldBeJSON ++ "].") >> fmap (operation (readMapFromString couldBeJSON)) readMap >>= saveMap

aPlace :: String -> IO ()
aPlace = mapOperation "sd: adding one or more locations using putative JSON"  insertPlaces

xPlace :: String -> IO ()
xPlace = mapOperation "sd: deleting a location using putative JSON" deletePlaces

aRoad :: String -> IO ()
aRoad = mapOperation "sd: adding/modifying one or more roads using putative JSON" upsertRoad

xRoad :: String -> IO ()
xRoad = mapOperation "sd: deleting one or more roads using putative JSON" deleteRoad

clear :: IO ()
clear = putStrLn "sd: Removing the system file." >> removeMap

maybeSaveMap :: Either String Map -> IO()
maybeSaveMap (Left e) = putStrLn $ "sd: ERROR: Getting map: " ++ e
maybeSaveMap (Right m) = saveMap m
        
initialise :: String -> IO ()
initialise filename =
  putStrLn ("sd: initialising a map using input from file [" ++ filename ++ "].") >> readMapFromFile filename >>= maybeSaveMap

initialiseJSON :: String -> IO ()
initialiseJSON couldBeJSON =
  putStrLn ("sd: initialising a map using putative JSON [" ++ couldBeJSON ++ "].") >> saveMap (readMapFromString couldBeJSON)

shortest :: String -> IO ()
shortest couldBeJSON = fmap encode (dijkstra (cs couldBeJSON)) >>= print
