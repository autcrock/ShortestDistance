Write-Output "EXAMPLE USAGE OF sd.exe"
Write-Output "BUILDING sd.exe and RUNNING TESTS"
stack test

Write-Output "SHOW sd.exe USAGE"
stack exec -- sd --help

Write-Output "INITIALISE PERSISTENT MAP USING DATA PROVIDED WITH TEST PROBLEM"
stack exec -- sd --initialise ./test/testmap2.json

Write-Output "Shortest path from A to B - see tests above for more comprehensive testing"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"B\"}'
Write-Output "Shortest path from A to H"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"H\"}'
Write-Output "Shortest path from A to C"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"C\"}'
Write-Output "Shortest path from H to B"
stack exec -- sd --shortest '{\"start\": \"H\", \"end\": \"B\"}'
Write-Output "Shortest path from H to C"
stack exec -- sd --shortest '{\"start\": \"H\", \"end\": \"C\"}'
Write-Output "Add destinations I and C to place H"
stack exec -- sd --addplace '{\"map\":[{\"place\":\"H\", \"destinations\": [ {\"to\": \"I\", \"distance\": 100}, {\"to\": \"C\", \"distance\": 24}]}]}'
Write-Output "Add new place U with destinations V and X"
stack exec -- sd --addplace '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"V\", \"distance\": 100}, {\"to\": \"X\", \"distance\": 32.5}]}]}'
Write-Output "Shortest path from H to I"
stack exec -- sd --shortest '{\"start\": \"H\", \"end\": \"I\"}'
Write-Output "Shortest path from A to C"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"C\"}'
Write-Output "Add road Z to U"
stack exec -- sd --addroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"Z\", \"distance\": 99.9999}]}]}'
Write-Output "Shortest path from Z to U"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"U\"}'
Write-Output "DELIBERATE FAILURE NEXT - THERE IS NO CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"A\"}'
Write-Output "Add connection between U and C to allow A and Z to connect."
stack exec -- sd --addroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"C\", \"distance\": 123}]}]}'
Write-Output "THERE IS NOW A CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"A\"}'
Write-Output "REMOVE connection between U and C to stop A and Z connection."
stack exec -- sd --xroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"C\", \"distance\": 123}]}]}'
Write-Output "DELIBERATE FAILURE NEXT - THERE IS AGAIN NO CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"A\"}'
Write-Output "Update distance between U and Z to 10"
stack exec -- sd --addroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"Z\", \"distance\": 10}]}]}'
Write-Output "Add new destination H to C in database"
stack exec -- sd --addroad '{\"map\":[{\"place\":\"H\", \"destinations\": [ {\"to\": \"C\", \"distance\": 2024.2}]}]}'
Write-Output "Check distance between A and C again"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"C\"}'
stack exec -- sd --clear
