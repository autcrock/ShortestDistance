# ShortestDistance

This program uses a simple list based Dijkstra algorithm to determine distances between vertices in a graph.  Neither the algorithm nor the underlying data structures are optimised for large real-world data.  The program can handle multiple disjoint graphs in it's persistent database, and will return JSON indicating that two places are not connected if asked to determine the distance between two unconnected places.

WINDOWS 10

To build it on Windows, just pull the git sources, ensure you have the Haskell stack utility installed and on your path then, at a Powershell prompt, type:

         .\test\RunCommandLineTests.ps1

LINUX or WINDOWS MSYS

To build it on Linux, just pull the git sources, ensure you have the Haskell stack utility installed and on your path then, at a Bash prompt, type:

         sh ./test/RunCommandLineTests.sh

This will build the program sd (or on Windows sd.exe), and run the tests.  It will also run a series of command line examples using sd.exe via stack exec (which adds considerable startup overheads on Windows).

MORE INFO

To get more detailed information about the command line options, read the above script, and also run:

        stack exec -- sd --help
