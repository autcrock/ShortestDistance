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
operation ["-s", from, to] = shortest from to >> exit
operation ["--shortest", from, to] = shortest  from to >> exit
operation ["-r"] = remove >> exit
operation ["--remove"] = remove >> exit
operation _ = usage >> exit


usage   = do
            putStrLn "Usage: sd [-shida] [file]"
            putStrLn "  sd -s p1 p2 - Find shortest path from p1 to p2 (v1, v2 are place names as strings) in a road network file."
            putStrLn "  sd -h - This message.  A road network is stored as JSON in a system file, for operations by the accompanying command line arguments."
            putStrLn "  sd -i filename - Initialise from a map definition in filename as JSON.  Remove system file first."
            putStrLn "  sd -d place - Delete this place as JSON from the system file."
            putStrLn "  sd -a place - Add this place as JSON to the system file."
            putStrLn "  sd -r - Remove the system file."
            
version = putStrLn "sd Alpha 0"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)