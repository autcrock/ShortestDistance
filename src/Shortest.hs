module Shortest (
    shortest
) where

    -- import qualified MapDefinitions

    -- data NodeState = Red | Yellow | Green
    -- data Destination
    -- data RYGPlace = Node String NodeState Double [Node]

    -- setupData :: IO RYGTree
    -- setupData =
    --     do
    --         map <- readMap
    --         workingList <- 

    -- placeToDatum :: Place -> RYGELement
    -- placeToDatum p =
    --     RYGElement (place p) Red (distance p)



    shortest :: String -> IO ()
    shortest couldBeJSON = putStrLn $ "shortest: finding the shortest route using putative JSON [" ++ couldBeJSON ++ "]."
