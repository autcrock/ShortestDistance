module Main where

import System.Environment
import System.Exit
     
import Lib ( add
            , clear
            , delete
            , initialise
            , road
            , shortest
            , xroad
            )

main :: IO ()
main = getArgs >>= operation >>= putStr
 
operation :: [String] -> IO b
operation ["-a", aString] = add aString >> exit
operation ["--addplace", aString] = add aString >> exit
operation ["-c"] = clear >> exit
operation ["--clear"] = clear >> exit
operation ["-d", aString] = delete aString >> exit
operation ["--deleteplace", aString] = delete aString >> exit
operation ["-h"] = usage >> exit
operation ["--help"] = usage >> exit
operation ["-i", filename] = initialise filename >> exit
operation ["--initialise", filename] = initialise filename >> exit
operation ["-r", aString] = road aString >> exit
operation ["--road", aString] = road aString >> exit
operation ["-s", aString] = shortest aString >> exit
operation ["--shortest", aString] = shortest aString >> exit
operation ["-v"] = version >> exit
operation ["--version"] = version >> exit
operation ["-x", aString] = xroad aString >> exit
operation ["--xroad", aString] = xroad aString >> exit
operation _ = usage >> exit

usage :: IO()
usage   = do
            putStrLn "Usage: sd [-shidxpac] [file]"
            putStrLn "  sd [-a, --addplace] place - Add this place as JSON to the system file."
            putStrLn "  sd [-c, --clear] - Remove the system file."
            putStrLn "  sd [-d, --deleteplace] place - Delete this place as JSON from the system file."
            putStrLn "  sd [-h, --help] - This message.  A road network is stored as JSON in a system file, for operations by the accompanying command line arguments."
            putStrLn "  sd [-i, --initialise] filename - Initialise from a map definition in filename as JSON.  Remove system file first."
            putStrLn "  sd [-r, --road] place - Add or modify a road from this place as JSON to the system file."
            putStrLn "  sd [-s, --shortest] StartEnd - StartEnd is as: '{\"start\": \"A\", \"end\": \"B\"}' - Find shortest path from place A to place B which are stored in a road network file."
            putStrLn "  sd [-v, --version] - Remove the system file."
            putStrLn "  sd [-x, --xroad] place - Delete the road specified as JSON for this place from the system file."

version :: IO()
version = putStrLn "sd Alpha 0"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitWith (ExitFailure 1)