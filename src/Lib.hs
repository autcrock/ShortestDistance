module Lib
    ( initialise
    , add
    , delete
    , shortest
    , remove
    ) where

import MapDefinitions (Map
            , getMapFromFile
            , addNodeToMap
            , deleteNodeFromMap
            , saveMap
            , removeMap)

import Shortest (dijkstra)
            
initialise :: String -> IO ()
initialise filename = 
    do
        putStrLn $ "sd: initialising a map using input from [" ++ filename ++ "]."
        map <- getMapFromFile filename
        maybeSaveMap map

add :: String -> IO ()
add couldBeJSON = putStrLn $ "sd: adding a location using putative JSON [" ++ couldBeJSON ++ "]."

delete :: String -> IO ()
delete couldBeJSON = putStrLn $ "sd: deleting a location using putative JSON [" ++ couldBeJSON ++ "]."

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
        print result
