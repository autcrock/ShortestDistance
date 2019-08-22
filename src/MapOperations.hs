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

mapOperation :: String -> (Map -> Map -> Map) -> String -> IO()
mapOperation message operation couldBeJSON =
    do putStrLn $ message ++ " [" ++ couldBeJSON ++ "]."
       amap <- readMap
       saveMap $ operation (readMapFromString couldBeJSON) amap

aPlace :: String -> IO ()
aPlace = mapOperation "sd: adding one or more locations using putative JSON"  insertPlaces

xPlace :: String -> IO ()
xPlace = mapOperation "sd: deleting a location using putative JSON" deletePlaces

aRoad :: String -> IO ()
aRoad = mapOperation "sd: adding/modifying one or more roads using putative JSON" upsertRoad

xRoad :: String -> IO ()
xRoad = mapOperation "sd: deleting one or more roads using putative JSON" deleteRoad

clear :: IO ()
clear = do
  putStrLn "sd: Removing the system file."
  removeMap

maybeSaveMap :: Either String Map -> IO()
maybeSaveMap (Left e) = do
  putStrLn $ "sd: ERROR: Getting map: " ++ e
  return ()
maybeSaveMap (Right m) = do
  saveMap m
  return ()
        
initialise :: String -> IO ()
initialise filename = do
  putStrLn $ "sd: initialising a map using input from file [" ++ filename ++ "]."
  m <- readMapFromFile filename
  maybeSaveMap m

initialiseJSON :: String -> IO ()
initialiseJSON couldBeJSON = do
  putStrLn $ "sd: initialising a map using putative JSON [" ++ couldBeJSON ++ "]."
  let mapToSave = readMapFromString couldBeJSON
  saveMap mapToSave

shortest :: String -> IO ()
shortest couldBeJSON = do
  result <- dijkstra (cs couldBeJSON)
  print $ fmap encode result
