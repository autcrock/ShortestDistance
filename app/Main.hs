{-# LANGUAGE OverloadedStrings        #-}
import MapOperations
    ( aPlace
    , clear
    , xPlace
    , initialise
    , aRoad
    , shortest
    , xRoad
    )
import System.Environment
import System.Exit
import PostgresMapDefinitions(test)

main :: IO ()
--main = test >>= print
main = getArgs >>= operation >>= putStr
 
operation :: [String] -> IO b
operation ["-ap", aString] = aPlace aString >> exit
operation ["--addplace", aString] = aPlace aString >> exit
operation ["-c"] = clear >> exit
operation ["--clear"] = clear >> exit
operation ["-xp", aString] = xPlace aString >> exit
operation ["--xplace", aString] = xPlace aString >> exit
operation ["-h"] = usage >> exit
operation ["--help"] = usage >> exit
operation ["-i", filename] = initialise filename >> exit
operation ["--initialise", filename] = initialise filename >> exit
operation ["--initialize", filename] = initialise filename >> exit
operation ["-ar", aString] = aRoad aString >> exit
operation ["--addroad", aString] = aRoad aString >> exit
operation ["-s", aString] = shortest aString >> exit
operation ["--shortest", aString] = shortest aString >> exit
operation ["-v"] = version >> exit
operation ["--version"] = version >> exit
operation ["-xr", aString] = xRoad aString >> exit
operation ["--xroad", aString] = xRoad aString >> exit
operation _ = usage >> exit

usage :: IO()
usage   = do
            putStrLn "Usage: sd switch [file|Map|StartEnd|nothing]"
            putStrLn ""
            putStrLn " Purpose: Manipulate and store a simple road network. Determine distances between locations with basic Dijkstra algorithm."
            putStrLn "          sd can handle multiple disjoint place/road maps.  It will tell you if two places are not connected."
            putStrLn "          NOTE: sd relies on its caller to ensure that road lengths are not negative."
            putStrLn ""
            putStrLn " Argument types:"
            putStrLn ""
            putStrLn "   - A map defines zero or more locations each with zero or more destinations: eg:"
            putStrLn "       '{\"map\":[{\"place\":\"A\", \"destinations\": [ {\"to\": \"B\", \"distance\": 100}, {\"to\": \"C\", \"distance\": 30}]}]}'"
            putStrLn "     Maps are used as inputs for map initialisation, place and road addition, deletion and updating where applicable."
            putStrLn "     Single destination maps only must be used for road addition, deletion or updating where applicable."
            putStrLn "     Multiple place and destination lists may be used for persistent map initialisation, or for addition of new places."
            putStrLn "     Empty destination and place lists may be used for addition and deletion of roads and places respectively."
            putStrLn ""
            putStrLn "   - A StartEnd pair is JSON defining a pair of places for calculation of distance: eg"
            putStrLn "       '{\"start\": \"A\", \"end\": \"B\"}'"
            putStrLn ""
            putStrLn " Command line switches:"
            putStrLn ""
            putStrLn "   sd [-ap, --addplace] Place - Add this place as JSON to the system file."
            putStrLn "   sd [-c, --clear] - Remove the system file."
            putStrLn "   sd [-xd, --xplace] Place - Delete this place as JSON from the system file."
            putStrLn "   sd [-h, --help] - This message.  A persistent road network is stored in a file for distance between locations."
            putStrLn "   sd [-i, --initialise] filename - Initialise from a map definition in filename as JSON.  Remove system file first."
            putStrLn "   sd [-ar, --addroad] Place - Add or modify a road from this place as JSON to the system file."
            putStrLn "   sd [-s, --shortest] StartEnd - Find shortest path from place A to place B which are stored in a road network file."
            putStrLn "                                  Returns either a JSON Distance or a message eg that the two places were not connected."
            putStrLn "   sd [-v, --version] - Remove the system file."
            putStrLn "   sd [-xr, --xroad] Place - Delete the road specified as JSON for this place from the system file."

version :: IO()
version = putStrLn "sd Alpha 0"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitWith (ExitFailure 1)
