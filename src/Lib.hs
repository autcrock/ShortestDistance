module Lib
    ( initialise
    , add
    , delete
    , shortest
    , remove
    ) where

import Data.Aeson (encode)

import MapDefinitions
    ( Map
    , getMapFromFile
    , readMapFromString
    , readMap
    , saveMap
    , removeMap
    , insertPlaces
    , deletePlaces
    )

import Shortest (dijkstra)
            
initialise :: String -> IO ()
initialise filename = 
    do
        putStrLn $ "sd: initialising a map using input from [" ++ filename ++ "]."
        m <- getMapFromFile filename
        maybeSaveMap m

add :: String -> IO ()
add couldBeJSON = 
    do
        putStrLn $ "sd: adding one or more locations using putative JSON [" ++ couldBeJSON ++ "]."
        let mapToInsert = readMapFromString couldBeJSON
        savedMap <- readMap
        let newMap = insertPlaces mapToInsert savedMap
        saveMap newMap

delete :: String -> IO ()
delete couldBeJSON =
    do
        putStrLn $ "sd: deleting a location using putative JSON [" ++ couldBeJSON ++ "]."
        let mapToDelete = readMapFromString couldBeJSON
        savedMap <- readMap
        let newMap = deletePlaces mapToDelete savedMap
        saveMap newMap

remove :: IO ()
remove = 
    do
        putStrLn "sd: Removing the system file."
        removeMap

maybeSaveMap :: Either String Map -> IO()
maybeSaveMap (Left e) =
    do
        putStrLn $ "sd: ERROR: Getting map: " ++ e
        return ()
maybeSaveMap (Right m) =
    do
        saveMap m
        return ()
        
shortest :: String -> IO ()
shortest couldBeJSON =
    do
--        putStrLn $ "sd: shortest input: " ++ couldBeJSON
        result <- dijkstra couldBeJSON
        print $ encode result
