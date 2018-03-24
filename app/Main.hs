module Main where

import System.Environment
import System.Exit
     
import Lib ( aplace
            , clear
            , xplace
            , initialise
            , aroad
            , shortest
            , xroad
            )

main :: IO ()
main = getArgs >>= operation >>= putStr
 
operation :: [String] -> IO b
operation ["-ap", aString] = aplace aString >> exit
operation ["--addplace", aString] = aplace aString >> exit
operation ["-c"] = clear >> exit
operation ["--clear"] = clear >> exit
operation ["-xp", aString] = xplace aString >> exit
operation ["--xplace", aString] = xplace aString >> exit
operation ["-h"] = usage >> exit
operation ["--help"] = usage >> exit
operation ["-i", filename] = initialise filename >> exit
operation ["--initialise", filename] = initialise filename >> exit
operation ["-ar", aString] = aroad aString >> exit
operation ["--addroad", aString] = aroad aString >> exit
operation ["-s", aString] = shortest aString >> exit
operation ["--shortest", aString] = shortest aString >> exit
operation ["-v"] = version >> exit
operation ["--version"] = version >> exit
operation ["-xr", aString] = xroad aString >> exit
operation ["--xroad", aString] = xroad aString >> exit
operation _ = usage >> exit

usage :: IO()
usage   = do
            putStrLn "Usage: sd [-shidxpac] [file]"
            putStrLn "  sd [-ap, --addplace] place - Add this place as JSON to the system file."
            putStrLn "  sd [-c, --clear] - Remove the system file."
            putStrLn "  sd [-xd, --deleteplace] place - Delete this place as JSON from the system file."
            putStrLn "  sd [-h, --help] - This message.  A road network is stored as JSON in a system file, for operations by the accompanying command line arguments."
            putStrLn "  sd [-i, --initialise] filename - Initialise from a map definition in filename as JSON.  Remove system file first."
            putStrLn "  sd [-ar, --addroad] place - Add or modify a road from this place as JSON to the system file."
            putStrLn "  sd [-s, --shortest] StartEnd - StartEnd is as: '{\"start\": \"A\", \"end\": \"B\"}' - Find shortest path from place A to place B which are stored in a road network file."
            putStrLn "  sd [-v, --version] - Remove the system file."
            putStrLn "  sd [-xr, --xroad] place - Delete the road specified as JSON for this place from the system file."

version :: IO()
version = putStrLn "sd Alpha 0"

exit :: IO a
exit = exitSuccess

die :: IO a
die = exitWith (ExitFailure 1)