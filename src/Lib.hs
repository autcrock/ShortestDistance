module Lib
    (
        aplace
        , clear
        , xplace
        , initialise
        , initialiseJSON
        , aroad
        , shortest
        , xroad
    ) where

import Data.Aeson (encode)
import Data.Either.Unwrap (isLeft, fromLeft, fromRight)

import MapDefinitions
    ( Map
    , getMapFromFile
    , readMapFromString
    , readMap
    , saveMap
    , removeMap
    , insertPlaces
    , deletePlaces
    , insertOrModifyRoad
    , deleteRoad
    )

import Shortest (dijkstra)
            
aplace :: String -> IO ()
aplace couldBeJSON = 
    do
        putStrLn $ "sd: adding one or more locations using putative JSON [" ++ couldBeJSON ++ "]."
        let mapToInsert = readMapFromString couldBeJSON
        savedMap <- readMap
        let newMap = insertPlaces mapToInsert savedMap
        saveMap newMap

clear :: IO ()
clear = 
    do
        putStrLn "sd: Removing the system file."
        removeMap

xplace :: String -> IO ()
xplace couldBeJSON =
    do
        putStrLn $ "sd: deleting a location using putative JSON [" ++ couldBeJSON ++ "]."
        let mapToDelete = readMapFromString couldBeJSON
        savedMap <- readMap
        let newMap = deletePlaces mapToDelete savedMap
        saveMap newMap

maybeSaveMap :: Either String Map -> IO()
maybeSaveMap (Left e) =
    do
        putStrLn $ "sd: ERROR: Getting map: " ++ e
        return ()
maybeSaveMap (Right m) =
    do
        saveMap m
        return ()
        
initialise :: String -> IO ()
initialise filename = 
    do
        putStrLn $ "sd: initialising a map using input from file [" ++ filename ++ "]."
        m <- getMapFromFile filename
        maybeSaveMap m

initialiseJSON :: String -> IO ()
initialiseJSON couldBeJSON = 
    do
        putStrLn $ "sd: initialising a map using putative JSON [" ++ couldBeJSON ++ "]."
        let mapToSave = readMapFromString couldBeJSON
        saveMap mapToSave

aroad :: String -> IO ()
aroad couldBeJSON = 
    do
        putStrLn $ "sd: adding/modifying one or more roads using putative JSON [" ++ couldBeJSON ++ "]."
        let mapToInsert = readMapFromString couldBeJSON
        savedMap <- readMap
        let newMap = insertOrModifyRoad mapToInsert savedMap
        saveMap newMap

shortest :: String -> IO ()
shortest couldBeJSON =
    do
        result <- dijkstra couldBeJSON
        let isItLeft = isLeft result
        if isItLeft
        then print $ encode (fromLeft result)
        else print $ encode (fromRight result)

xroad :: String -> IO ()
xroad couldBeJSON =
    do
        putStrLn $ "sd: deleting one or more roads using putative JSON [" ++ couldBeJSON ++ "]."
        let mapToDelete = readMapFromString couldBeJSON
        savedMap <- readMap
        let newMap = deleteRoad mapToDelete savedMap
        saveMap newMap

        