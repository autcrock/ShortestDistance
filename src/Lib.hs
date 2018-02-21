module Lib
    ( initialise
    , add
    , delete
    , shortest
    , remove
    ) where

import Shortest (shortest)
import MapDefinitions (Map
            , getMapFromFile
            , addNodeToMap
            , deleteNodeFromMap
            , saveMap
            , removeMap)

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
remove = putStrLn "sd: Removing the system file."

maybeSaveMap :: Either String Map -> IO()
maybeSaveMap (Left e) =
    do
        putStrLn $ "sd: ERROR: Getting map: " ++ e
        return ()
maybeSaveMap (Right m) =
    do
        saveMap m
        return ()
