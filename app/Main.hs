module Main where

import System.Environment
import System.Exit
     
import Lib ( initialise
            , add
            , delete
            , shortest
            , remove)

main :: IO ()
main = getArgs >>= operation >>= putStr
 
operation :: [String] -> IO b
operation ["-h"] = usage >> exit
operation ["--help"] = usage >> exit
operation ["-v"] = version >> exit
operation ["--version"] = version >> exit
operation ["-i", filename] = initialise filename >> exit
operation ["--initialise", filename] = initialise filename >> exit
operation ["-a", aString] = add aString >> exit
operation ["--add", aString] = add aString >> exit
operation ["-d", aString] = delete aString >> exit
operation ["--delete", aString] = delete aString >> exit
operation ["-s", aString] = shortest aString >> exit
operation ["--shortest", aString] = shortest aString >> exit
operation ["-r"] = remove >> exit
operation ["--remove"] = remove >> exit
operation _ = usage >> exit

usage :: IO()
usage   = do
            putStrLn "Usage: sd [-shida] [file]"
            putStrLn "  sd [-s, --shortest] StartEnd - StartEnd is as: '{\"start\": \"A\", \"end\": \"B\"}' - Find shortest path from place A to place B which are stored in a road network file."
            putStrLn "  sd [-h, --help] - This message.  A road network is stored as JSON in a system file, for operations by the accompanying command line arguments."
            putStrLn "  sd [-i, --initialise] filename - Initialise from a map definition in filename as JSON.  Remove system file first."
            putStrLn "  sd [-d, --delete] place - Delete this place as JSON from the system file."
            putStrLn "  sd [-a, --add] place - Add this place as JSON to the system file."
            putStrLn "  sd [-r, --remove] - Remove the system file."

version :: IO()
version = putStrLn "sd Alpha 0"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitWith (ExitFailure 1)