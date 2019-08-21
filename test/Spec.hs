{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (encode)
import Data.Text (Text)
import Data.Either.Unwrap (isLeft, fromLeft, fromRight)
import Data.String.Conversions (cs)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MapOperations (
    aPlace
    , clear
    , xPlace
    , initialise
    , initialiseJSON
    , aRoad
    , xRoad
    )

import MapDefinitions (Map(..), StartEnd(..), readMap)
import Shortest (dijkstra, UnusualResult(..), Distance(..))

mapInputDataFile :: String
mapInputDataFile = "./test/testmap2.json"

boolToResult :: Bool -> Text
boolToResult br = if br then "PASS" else "FAIL"

boolToFinalResult :: Bool -> String
boolToFinalResult br = if br then "ALL TESTS PASSED" else "ONE OR MORE TESTS FAILED"

expected :: (Text, Text, Either UnusualResult Double) -> IO Bool
expected (from, to, expectedDistance) =
    do  
        let enced = encode StartEnd { start = from, end = to }
            expectedD = if isLeft expectedDistance
                        then Left (fromLeft expectedDistance)
                        else Right Distance {distance = fromRight expectedDistance}
        fsd <- dijkstra $ cs enced
        let forwardTrue = if isLeft fsd
                            then fromLeft fsd == fromLeft expectedD
                            else fromRight fsd == fromRight expectedD
        rsd <- dijkstra $ cs $ encode StartEnd { start = to, end = from }
        let reverseTrue = if isLeft rsd
            then fromLeft rsd == fromLeft expectedD
            else fromRight rsd == fromRight expectedD
        TIO.putStrLn $ T.concat [
            "From place [", from,"] to [", to,
            "] the expected distance is: [", cs (show expectedD),
            "]. Calculated forward: [", cs (show $ fromRight fsd),
            "] ", boolToResult forwardTrue,
            ", and reverse: [", cs (show $ fromRight rsd),
            "] ", boolToResult reverseTrue,
            "."
            ]
        return (forwardTrue && reverseTrue)

main :: IO ()
main = 
    do 
        putStrLn $ "Test suite initialising persistent map : " ++ mapInputDataFile
        initialise mapInputDataFile

        putStrLn $ "Test suite running through permutations of test locations A to H " ++ mapInputDataFile
        let dijkstraTestData =  [
                ("A", "A", Right 0), ("A", "B", Right 100), ("A", "C", Right 30), ("A", "D", Right 230), ("A", "E", Right 310), ("A", "F", Right 360), ("A", "G", Right 370), ("A", "H", Right 320)
                , ("B", "B", Right 0), ("B", "C", Right 130), ("B", "D", Right 330), ("B", "E", Right 350), ("B", "F", Right 300), ("B", "G", Right 370), ("B", "H", Right 380)
                , ("C", "C", Right 0), ("C", "D", Right 200), ("C", "E", Right 280), ("C", "F", Right 330), ("C", "G", Right 340), ("C", "H", Right 290)
                , ("D", "D", Right 0), ("D", "E", Right 80), ("D", "F", Right 130), ("D", "G", Right 140), ("D", "H", Right 90)
                , ("E", "E", Right 0), ("E", "F", Right 50), ("E", "G", Right 80), ("E", "H", Right 30)
                , ("F", "F", Right 0), ("F", "G", Right 70), ("F", "H", Right 80)
                , ("G", "G", Right 0), ("G", "H", Right 50)
                , ("H", "H", Right 0)
                ]
        results <- mapM expected dijkstraTestData

        putStrLn $ "Test suite removing persistent map : " ++ mapInputDataFile
        clear

        let part1 = "{\"map\":[]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part1: " ++ part1
        initialiseJSON part1
        let part2 = "{\"map\":[{\"place\":\"A\", \"isConnectedTo\": [ {\"at\": \"B\", \"howFar\": 100}, {\"at\": \"C\", \"howFar\": 30}]}]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part2: " ++ part2
        aPlace part2
        let part3 = "{\"map\":[{ \
            \\"place\":\"B\",\
            \\"isConnectedTo\": [\
            \    {\"at\": \"F\", \"howFar\": 300}]\
            \},\
            \{\
            \    \"place\":\"C\",\
            \    \"isConnectedTo\": [\
            \        {\"at\": \"D\", \"howFar\": 200}]\
            \},\
            \{\
            \    \"place\":\"D\",\
            \    \"isConnectedTo\": [\
            \        {\"at\": \"H\", \"howFar\": 90},\
            \        {\"at\": \"E\", \"howFar\": 80}]\
            \},\
            \{\
            \    \"place\":\"E\",\
            \    \"isConnectedTo\": [\
            \        {\"at\": \"H\", \"howFar\": 30},\
            \        {\"at\": \"G\", \"howFar\": 150},\
            \        {\"at\": \"F\", \"howFar\": 50}]\
            \},\
            \{\
            \    \"place\":\"F\",\
            \    \"isConnectedTo\": [\
            \        {\"at\": \"G\", \"howFar\": 70}]\
            \},\
            \{\
            \    \"place\":\"G\",\
            \    \"isConnectedTo\": [\
            \        {\"at\": \"H\", \"howFar\": 50}]\
            \}\
            \]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part2: " ++ part3
        aPlace part3
        
        putStrLn $ "Test suite running through permutations of test locations A to H " ++ mapInputDataFile
        results1 <- mapM expected dijkstraTestData

        let part4 = "{\"map\":[{ \
            \\"place\":\"H\",\
            \\"isConnectedTo\": []\
            \}\
            \]}"
        putStrLn $ "Test suite deleting location H from map " ++ part4
        xPlace part4
        let dijkstraTestDataWithoutH =  [
                ("A", "A", Right 0), ("A", "B", Right 100), ("A", "C", Right 30), ("A", "D", Right 230), ("A", "E", Right 310), ("A", "F", Right 360), ("A", "G", Right 430)
                , ("B", "B", Right 0), ("B", "C", Right 130), ("B", "D", Right 330), ("B", "E", Right 350), ("B", "F", Right 300), ("B", "G", Right 370)
                , ("C", "C", Right 0), ("C", "D", Right 200), ("C", "E", Right 280), ("C", "F", Right 330), ("C", "G", Right 400)
                , ("D", "D", Right 0), ("D", "E", Right 80), ("D", "F", Right 130), ("D", "G", Right 200)
                , ("E", "E", Right 0), ("E", "F", Right 50), ("E", "G", Right 120)
                , ("F", "F", Right 0), ("F", "G", Right 70)
                , ("G", "G", Right 0)
                ]
        putStrLn "Test suite running through permutations of test locations A to G without H in graph."
        results2 <- mapM expected dijkstraTestDataWithoutH

        let part5 = "{\"map\":[{ \
            \\"place\":\"C\",\
            \\"isConnectedTo\": []\
            \}\
            \,{ \
            \\"place\":\"A\",\
            \\"isConnectedTo\": []\
            \}\
            \]}"

        putStrLn $ "Test suite deleting locations C and A from map " ++ part5
        xPlace part5

        let dijkstraTestDataWithoutHCandA =  [
                ("B", "B", Right 0), ("B", "D", Right 430), ("B", "E", Right 350), ("B", "F", Right 300), ("B", "G", Right 370)
                , ("D", "D", Right 0), ("D", "E", Right 80), ("D", "F", Right 130), ("D", "G", Right 200)
                , ("E", "E", Right 0), ("E", "F", Right 50), ("E", "G", Right 120)
                , ("F", "F", Right 0), ("F", "G", Right 70)
                , ("G", "G", Right 0)
                ]

        putStrLn "Test suite running through permutations of test locations A to G without H in graph."
        results3 <- mapM expected dijkstraTestDataWithoutHCandA

        putStrLn $ "Test suite initialising persistent map by additions - road testing. Part1: " ++ part1
        initialiseJSON part1
        let part6 = "{\"map\":[{\"place\":\"A\", \"isConnectedTo\": []}]}"
        putStrLn $ "Test suite initialising persistent map by additions - road testing. Part6: " ++ part6
        aPlace part6

        let part7 = "{\"map\":[{\"place\":\"A\", \"isConnectedTo\": [ {\"at\": \"B\", \"howFar\": 100}]}]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part7: " ++ part7
        aRoad part7

        let part8 = "{\"map\":[{\"place\":\"A\", \"isConnectedTo\": [ {\"at\": \"C\", \"howFar\": 30}]}]}"
        putStrLn $ "Test suite initialising persistent map by additions. Part8: " ++ part8
        aRoad part8

        let dijkstraTestDataRoadInsertion =  [
                ("A", "A", Right 0), ("A", "B", Right 100), ("A", "C", Right 30)
                ,("B", "B", Right 0), ("B", "C", Right 130)
                ,("C", "C", Right 0)
                ]

        putStrLn "Test suite running through permutations of road insertion test data."
        results4 <- mapM expected dijkstraTestDataRoadInsertion

        let part9 = "{\"map\":[{\"place\":\"A\", \"isConnectedTo\": [ {\"at\": \"B\", \"howFar\": 600}]}]}"
        putStrLn $ "Test suite updating road A to B map by additions. Part9: " ++ part9
        aRoad part9

        let dijkstraTestDataRoadInsertionAfterUpdatingAtoB =  [
                ("A", "A", Right 0), ("A", "B", Right 600), ("A", "C", Right 30)
                ,("B", "B", Right 0), ("B", "C", Right 630)
                ,("C", "C", Right 0)
                ]

        putStrLn "Test suite running through permutations of road insertion test data."
        results5 <- mapM expected dijkstraTestDataRoadInsertionAfterUpdatingAtoB

        let part10 = "{\"map\":[{\"place\":\"A\", \"isConnectedTo\": [ {\"at\": \"C\", \"howFar\": 0}]}]}"
        putStrLn $ "Test suite updating road A to C map by additions as zero length road. Part10: " ++ part10
        aRoad part10

        let dijkstraTestDataRoadInsertionAfterUpdatingAtoC =  [
                ("A", "A", Right 0), ("A", "B", Right 600), ("A", "C", Right 0)
                ,("B", "B", Right 0), ("B", "C", Right 600)
                ,("C", "C", Right 0)
                ]

        putStrLn "Test suite running through permutations of road insertion test data."
        results6 <- mapM expected dijkstraTestDataRoadInsertionAfterUpdatingAtoC

        let part11 = "{\"map\":[{\"place\":\"C\", \"isConnectedTo\": [ {\"at\": \"A\", \"howFar\": 999}]}]}"
        putStrLn $ "Test suite updating road A to C by additions in reverse. Part10: " ++ part11
        aRoad part11

        let part12 = "{\"map\":[{\"place\":\"D\", \"isConnectedTo\": [ {\"at\": \"A\", \"howFar\": 108}]}]}"
        putStrLn $ "Test suite inserting road A to D by additions in reverse. Part12: " ++ part12
        aRoad part12

        let dijkstraTestDataRoadInsertionAfterUpdatingCtoAAndInsertingAtoD =  [
                ("A", "A", Right 0), ("A", "B", Right 600), ("A", "C", Right 999), ("A", "D", Right 108)
                ,("B", "B", Right 0), ("B", "C", Right 1599), ("B", "D", Right 708)
                ,("C", "C", Right 0), ("C", "D", Right 1107)
                ,("C", "C", Right 0)
                ]

        putStrLn "Test suite running through permutations of road insertion test data."
        results7 <- mapM expected dijkstraTestDataRoadInsertionAfterUpdatingCtoAAndInsertingAtoD

        putStrLn $ "Test suite deleting road A to C from persistent map. Part8: " ++ part8
        xRoad part8

        let part13 = "{\"map\":[{\"place\":\"B\", \"isConnectedTo\": [ {\"at\": \"A\", \"howFar\": 100}]}]}"
        putStrLn $ "Test suite deleting reversed road A to B from persistent map. Part11: " ++ part13
        xRoad part13

        putStrLn $ "Test suite deleting reversed road A to D from persistent map. Part12: " ++ part12
        xRoad part12

        let part14 = "{\"map\":[{\"place\":\"A\", \"isConnectedTo\": []}]}"
        putStrLn $ "Test suite deleting place A from persistent map. Part14: " ++ part14
        xPlace part14

        putStrLn $ "Test suite checking that the persistent map is now empty: " ++ mapInputDataFile
        m <- readMap
        let results8 = [m == MapDefinitions.Map {MapDefinitions.map=[]}]

        putStrLn $ "Test suite removing persistent map : " ++ mapInputDataFile
        clear

        putStrLn "========================================================="
        putStrLn $ "Test suite overall results: " ++ boolToFinalResult (and 
            (results ++ results1 ++ results2 ++ results3 ++ results4
             ++ results5 ++ results6 ++ results7 ++ results8))
        putStrLn "========================================================="
