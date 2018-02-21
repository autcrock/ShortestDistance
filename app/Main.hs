module Main where

import System.Environment
import System.Exit
     
import Lib ( initialise
            , add
            , delete
            , shortest
            , remove)

main :: IO ()
main = getArgs >>= operation >>= putStr . tac
 
tac  = unlines . reverse . lines

operation ["-h"] = usage   >> exit
operation ["-v"] = version >> exit
operation ["-i", filename] = initialise filename >> exit
operation ["-a", aString] = add aString >> exit
operation ["-d", aString] = delete aString >> exit
operation ["-s", aString] = shortest aString >> exit
operation ["-r"] = remove >> exit

operation fs     = concat `fmap` mapM readFile fs

usage   = do
            putStrLn "Usage: sd [-shida] [file]"
            putStrLn "  sd -s p1 p2 - Find shortest path from p1 to p2 (v1, v2 are place names as strings) in a road network file."
            putStrLn "  sd -h - This message.  A road network is storeed as JSON in a system file, for operations by the accompanying command line arguments."
            putStrLn "  sd -i filename - Initialise from a map definition in filename as JSON.  Remove system file first."
            putStrLn "  sd -d place - Delete this place as JSON from the system file."
            putStrLn "  sd -a place - Add this place as JSON to the system file."
            putStrLn "  sd -r - Remove the system file."
            
version = putStrLn "sd Alpha 0"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)