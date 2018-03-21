import Lib ( initialise
            , add
            , delete
            , remove)

import Graph(
    StartEnd(..)
    , Distance(..))
import Shortest (dijkstra)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8(unpack)

expected :: String -> String -> Double -> IO ()
expected from to expectedDistance =
    do  
        let enced = encode StartEnd { start = from, end = to }
--        print $ unpack enced
        fsd <- dijkstra $ unpack enced
        let ed = Distance {distance = expectedDistance}
        rsd <- dijkstra $ unpack (encode StartEnd { start = to, end = from })
        print ( "[" ++ from ++  "] to [" ++ to
            ++ "], expected distance: [" ++ show expectedDistance
            ++ "]. Forward: [" ++ show (distance fsd)
            ++ "]. Reverse: [" ++ show (distance rsd)
            ++ "]. Forward correct [" ++ show (ed == fsd)
            ++ "]. Reverse correct [" ++ show (ed == rsd) ++ "]" )

main :: IO ()
main = 
    do 
        putStrLn $ "Test suite initialising persistent map : " ++ mapData
        initialise mapData

        putStrLn $ "Test suite running through permutations of test locations A to H " ++ mapData
        expected "A" "A" 0
        expected "A" "B" 100
        expected "A" "C" 30
        expected "A" "D" 230
        expected "A" "E" 310
        expected "A" "F" 360
        expected "A" "G" 370
        expected "A" "H" 320

        expected "B" "B" 0
        expected "B" "C" 130
        expected "B" "D" 330
        expected "B" "E" 350
        expected "B" "F" 300
        expected "B" "G" 370
        expected "B" "H" 380

        expected "C" "C" 0
        expected "C" "D" 200
        expected "C" "E" 280
        expected "C" "F" 330
        expected "C" "G" 340
        expected "C" "H" 290
        
        expected "D" "D" 0
        expected "D" "E" 80
        expected "D" "F" 130
        expected "D" "G" 140
        expected "D" "H" 90

        expected "E" "E" 0
        expected "E" "F" 50
        expected "E" "G" 80
        expected "E" "H" 30

        expected "F" "F" 0
        expected "F" "G" 70
        expected "F" "H" 80

        expected "G" "G" 0
        expected "G" "H" 50

        expected "H" "H" 0

mapData :: String
mapData = "./test/testmap2.json"