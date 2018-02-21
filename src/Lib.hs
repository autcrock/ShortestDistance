module Lib
    ( initialise
    , add
    , delete
    , shortest
    , remove
    ) where

initialise :: String -> IO ()
initialise filename = putStrLn $ "sd: initialising a map using input from [" ++ filename ++ "]."

add :: String -> IO ()
add couldBeJSON = putStrLn $ "sd: adding a location using putative JSON [" ++ couldBeJSON ++ "]."

delete :: String -> IO ()
delete couldBeJSON = putStrLn $ "sd: deleting a location using putative JSON [" ++ couldBeJSON ++ "]."

shortest :: String -> IO ()
shortest couldBeJSON = putStrLn $ "sd: finding the shortest route using putative JSON [" ++ couldBeJSON ++ "]."

remove :: IO ()
remove = putStrLn "sd: Removing the system file."
