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
import Data.Foldable (foldrM)

expected :: (String, String, Double) -> IO Bool
expected (from, to, expectedDistance) =
    do  
        let enced = encode StartEnd { start = from, end = to }
            expectedD = Distance {distance = expectedDistance}
--        print $ unpack enced
        fsd <- dijkstra $ unpack enced
        let forwardTrue = fsd == expectedD
        rsd <- dijkstra $ unpack (encode StartEnd { start = to, end = from })
        let reverseTrue = rsd == expectedD
        print ( "[" ++ from ++  "] to [" ++ to
            ++ "],     , (distance: [" ++ show expectedDistance
            ++ "]. Forward: [" ++ show (distance fsd)
            ++ "]. Reverse: [" ++ show (distance rsd)
            ++ "]. Forward correct [" ++ show forwardTrue
            ++ "]. Reverse correct [" ++ show reverseTrue ++ "]" )
        return (forwardTrue && reverseTrue)

main :: IO ()
main = 
    do 
        putStrLn $ "Test suite initialising persistent map : " ++ mapData
        initialise mapData

        putStrLn $ "Test suite running through permutations of test locations A to H " ++ mapData
        let dijkstraTestData =  [
                ("A", "A", 0)
                , ("A", "B", 100)
                , ("A", "C", 30)
                , ("A", "D", 230)
                , ("A", "E", 310)
                , ("A", "F", 360)
                , ("A", "G", 370)
                , ("A", "H", 320)
                , ("B", "B", 0)
                , ("B", "C", 130)
                , ("B", "D", 330)
                , ("B", "E", 350)
                , ("B", "F", 300)
                , ("B", "G", 370)
                , ("B", "H", 380)
                , ("C", "C", 0)
                , ("C", "D", 200)
                , ("C", "E", 280)
                , ("C", "F", 330)
                , ("C", "G", 340)
                , ("C", "H", 290)
                , ("D", "D", 0)
                , ("D", "E", 80)
                , ("D", "F", 130)
                , ("D", "G", 140)
                , ("D", "H", 90)
                , ("E", "E", 0)
                , ("E", "F", 50)
                , ("E", "G", 80)
                , ("E", "H", 30)
                , ("F", "F", 0)
                , ("F", "G", 70)
                , ("F", "H", 80)
                , ("G", "G", 0)
                , ("G", "H", 50)
                , ("H", "H", 0)
                ]
        results <- mapM expected dijkstraTestData
        let r = and results
        putStrLn $ "Overall result is: " ++ show r

mapData :: String
mapData = "./test/testmap2.json"