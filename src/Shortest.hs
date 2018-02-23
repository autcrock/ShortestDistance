module Shortest (
    shortest
) where

    import MapDefinitions

    data NodeState = Red | Yellow | Green
    data Node = Node String NodeState Double [Node]

    shortest :: String -> IO ()
    shortest couldBeJSON = putStrLn $ "shortest: finding the shortest route using putative JSON [" ++ couldBeJSON ++ "]."
