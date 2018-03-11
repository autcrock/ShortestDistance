import Lib ( initialise
            , add
            , delete
            , shortest
            , remove)
-- expected :: String -> String -> Double -> Boolean
-- expected from to expectedDistance =
--     shortest from to == 

main :: IO ()
main = 
    do 
        putStrLn $ "Test suite initialising persistent map : " ++ mapData
        initialise mapData

        putStrLn $ "Test suite running through permutations of test locations A to H " ++ mapData
        shortest "A" "A"
        shortest "A" "B"
        shortest "A" "C"
        shortest "A" "D"
        shortest "A" "E"
        shortest "A" "F"
        shortest "A" "G"
        shortest "A" "H"

        shortest "B" "A"
        shortest "B" "B"
        shortest "B" "C"
        shortest "B" "D"
        shortest "B" "E"
        shortest "B" "F"
        shortest "B" "G"
        shortest "B" "H"

        shortest "C" "A"
        shortest "C" "B"
        shortest "C" "C"
        shortest "C" "D"
        shortest "C" "E"
        shortest "C" "F"
        shortest "C" "G"
        shortest "C" "H"
        
        shortest "D" "A"
        shortest "D" "B"
        shortest "D" "C"
        shortest "D" "D"
        shortest "D" "E"
        shortest "D" "F"
        shortest "D" "G"
        shortest "D" "H"

        shortest "E" "A"
        shortest "E" "B"
        shortest "E" "C"
        shortest "E" "D"
        shortest "E" "E"
        shortest "E" "F"
        shortest "E" "G"
        shortest "E" "H"

        shortest "F" "A"
        shortest "F" "B"
        shortest "F" "C"
        shortest "F" "D"
        shortest "F" "E"
        shortest "F" "F"
        shortest "F" "G"
        shortest "F" "H"

        shortest "G" "A"
        shortest "G" "B"
        shortest "G" "C"
        shortest "G" "D"
        shortest "G" "E"
        shortest "G" "F"
        shortest "G" "G"
        shortest "G" "H"

        shortest "H" "A"
        shortest "H" "B"
        shortest "H" "C"
        shortest "H" "D"
        shortest "H" "E"
        shortest "H" "F"
        shortest "H" "G"
        shortest "H" "H"

mapData :: String
mapData = "./test/testmap2.json"