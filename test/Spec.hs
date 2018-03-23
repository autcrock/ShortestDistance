import Lib ( initialise
            , initialiseJSON
            , add
            , delete
            , remove)

import Graph(
    StartEnd(..)
    , Distance(..))
import Shortest (dijkstra)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8(unpack)

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
        putStrLn $ "Test suite initialising persistent map : " ++ mapInputDataFile
        initialise mapInputDataFile

        putStrLn $ "Test suite running through permutations of test locations A to H " ++ mapInputDataFile
        let dijkstraTestData =  [
                ("A", "A", 0), ("A", "B", 100), ("A", "C", 30), ("A", "D", 230), ("A", "E", 310), ("A", "F", 360), ("A", "G", 370), ("A", "H", 320)
                , ("B", "B", 0), ("B", "C", 130), ("B", "D", 330), ("B", "E", 350), ("B", "F", 300), ("B", "G", 370), ("B", "H", 380)
                , ("C", "C", 0), ("C", "D", 200), ("C", "E", 280), ("C", "F", 330), ("C", "G", 340), ("C", "H", 290)
                , ("D", "D", 0), ("D", "E", 80), ("D", "F", 130), ("D", "G", 140), ("D", "H", 90)
                , ("E", "E", 0), ("E", "F", 50), ("E", "G", 80), ("E", "H", 30)
                , ("F", "F", 0), ("F", "G", 70), ("F", "H", 80)
                , ("G", "G", 0), ("G", "H", 50)
                , ("H", "H", 0)
                ]
        results <- mapM expected dijkstraTestData

        putStrLn $ "Test suite removing persistent map : " ++ mapInputDataFile
        remove

        let part1 = "{\"map\":[]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part1: " ++ part1
        initialiseJSON part1
        let part2 = "{\"map\":[{\"place\":\"A\", \"destinations\": [ {\"to\": \"B\", \"distance\": 100}, {\"to\": \"C\", \"distance\": 30}]}]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part2: " ++ part2
        add part2
        let part3 = "{\"map\":[{ \
            \\"place\":\"B\",\
            \\"destinations\": [\
            \    {\"to\": \"F\", \"distance\": 300}]\
            \},\
            \{\
            \    \"place\":\"C\",\
            \    \"destinations\": [\
            \        {\"to\": \"D\", \"distance\": 200}]\
            \},\
            \{\
            \    \"place\":\"D\",\
            \    \"destinations\": [\
            \        {\"to\": \"H\", \"distance\": 90},\
            \        {\"to\": \"E\", \"distance\": 80}]\
            \},\
            \{\
            \    \"place\":\"E\",\
            \    \"destinations\": [\
            \        {\"to\": \"H\", \"distance\": 30},\
            \        {\"to\": \"G\", \"distance\": 150},\
            \        {\"to\": \"F\", \"distance\": 50}]\
            \},\
            \{\
            \    \"place\":\"F\",\
            \    \"destinations\": [\
            \        {\"to\": \"G\", \"distance\": 70}]\
            \},\
            \{\
            \    \"place\":\"G\",\
            \    \"destinations\": [\
            \        {\"to\": \"H\", \"distance\": 50}]\
            \}\
            \]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part2: " ++ part3
        add part3
        
        putStrLn $ "Test suite running through permutations of test locations A to H " ++ mapInputDataFile
        results1 <- mapM expected dijkstraTestData

        let part4 = "{\"map\":[{ \
            \\"place\":\"H\",\
            \\"destinations\": []\
            \}\
            \]}"
        putStrLn $ "Test suite deleting location H from map " ++ part4
        delete part4
        let dijkstraTestDataWithoutH =  [
                ("A", "A", 0), ("A", "B", 100), ("A", "C", 30), ("A", "D", 230), ("A", "E", 310), ("A", "F", 360), ("A", "G", 430)
                , ("B", "B", 0), ("B", "C", 130), ("B", "D", 330), ("B", "E", 350), ("B", "F", 300), ("B", "G", 370)
                , ("C", "C", 0), ("C", "D", 200), ("C", "E", 280), ("C", "F", 330), ("C", "G", 400)
                , ("D", "D", 0), ("D", "E", 80), ("D", "F", 130), ("D", "G", 200)
                , ("E", "E", 0), ("E", "F", 50), ("E", "G", 120)
                , ("F", "F", 0), ("F", "G", 70)
                , ("G", "G", 0)
                ]
        putStrLn "Test suite running through permutations of test locations A to G without H in graph."
        results2 <- mapM expected dijkstraTestDataWithoutH

        let part5 = "{\"map\":[{ \
            \\"place\":\"C\",\
            \\"destinations\": []\
            \}\
            \,{ \
            \\"place\":\"A\",\
            \\"destinations\": []\
            \}\
            \]}"

        putStrLn $ "Test suite deleting locations C and A from map " ++ part5
        delete part5

        let dijkstraTestDataWithoutHCandA =  [
                ("B", "B", 0), ("B", "D", 430), ("B", "E", 350), ("B", "F", 300), ("B", "G", 370)
                , ("D", "D", 0), ("D", "E", 80), ("D", "F", 130), ("D", "G", 200)
                , ("E", "E", 0), ("E", "F", 50), ("E", "G", 120)
                , ("F", "F", 0), ("F", "G", 70)
                , ("G", "G", 0)
                ]

        putStrLn "Test suite running through permutations of test locations A to G without H in graph."
        results3 <- mapM expected dijkstraTestDataWithoutHCandA

        let r = and (results ++ results1 ++ results2 ++ results3)
        putStrLn $ "Overall result is: " ++ show r
        putStrLn $ "Test suite removing persistent map : " ++ mapInputDataFile
        
        remove

mapInputDataFile :: String
mapInputDataFile = "./test/testmap2.json"