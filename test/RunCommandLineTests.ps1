Write-Output "--------------------------------------------------------------------"
Write-Output ""
Write-Output "EXAMPLE USAGE OF sd.exe"
Write-Output "--------------------------------------------------------------------"
Write-Output ""
Write-Output "BUILDING sd.exe and RUNNING TESTS"
stack test

Write-Output "--------------------------------------------------------------------"
Write-Output ""
Write-Output "SHOW sd.exe USAGE"
stack exec -- sd --help

Write-Output "--------------------------------------------------------------------"
Write-Output ""
Write-Output "INITIALISE PERSISTENT MAP USING DATA PROVIDED WITH TEST PROBLEM"
stack exec -- sd --initialise ./test/testmap2.json

Write-Output "--------------------------------------------------------------------"
Write-Output ""
Write-Output "SHORTEST PATH FROM A to B - see tests above for more comprehensive testing"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"B\"}'
Write-Output ""
Write-Output "SHORTEST PATH FROM A to H"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"H\"}'
Write-Output ""
Write-Output "SHORTEST PATH FROM A to C"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"C\"}'
Write-Output ""
Write-Output "SHORTEST PATH FROM H to B"
stack exec -- sd --shortest '{\"start\": \"H\", \"end\": \"B\"}'
Write-Output ""
Write-Output "SHORTEST PATH FROM H to C"
stack exec -- sd --shortest '{\"start\": \"H\", \"end\": \"C\"}'
Write-Output ""
Write-Output "ADD destinations I and C to place H"
stack exec -- sd --addplace '{\"map\":[{\"place\":\"H\", \"destinations\": [ {\"to\": \"I\", \"distance\": 100}, {\"to\": \"C\", \"distance\": 24}]}]}'
Write-Output ""
Write-Output "ADD new place U with destinations V and X"
stack exec -- sd --addplace '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"V\", \"distance\": 100}, {\"to\": \"X\", \"distance\": 32.5}]}]}'
Write-Output ""
Write-Output "SHORTEST PATH FROM H to I"
stack exec -- sd --shortest '{\"start\": \"H\", \"end\": \"I\"}'
Write-Output ""
Write-Output "SHORTEST PATH FROM A to G"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"G\"}'
Write-Output ""
Write-Output "DELETE PLACE H AND ROADS TO ITS NEIGHBOURS - ALTERS DISTANCE BETWEEN A and G."
stack exec -- sd --xplace '{\"map\":[{\"place\":\"H\", \"destinations\": []}]}'
Write-Output ""
Write-Output "SHORTEST PATH FROM A to G"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"G\"}'
Write-Output ""
Write-Output "SHORTEST PATH FROM A to C"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"C\"}'
Write-Output ""
Write-Output "ADD road Z to U"
stack exec -- sd --addroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"Z\", \"distance\": 99.9999}]}]}'
Write-Output ""
Write-Output "SHORTEST PATH FROM Z to U"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"U\"}'
Write-Output ""
Write-Output "DELIBERATE FAILURE NEXT - THERE IS NO CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"A\"}'
Write-Output ""
Write-Output "ADD connection between U and C to allow A and Z to connect."
stack exec -- sd --addroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"C\", \"distance\": 123}]}]}'
Write-Output ""
Write-Output "THERE IS NOW A CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"A\"}'
Write-Output ""
Write-Output "REMOVE connection between U and C to stop A and Z connection."
stack exec -- sd --xroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"C\", \"distance\": 123}]}]}'
Write-Output ""
Write-Output "DELIBERATE FAILURE NEXT - THERE IS AGAIN NO CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"A\"}'
Write-Output ""
Write-Output "UPDATE DISTANCE BETWEEN U and Z to 10"
stack exec -- sd --addroad '{\"map\":[{\"place\":\"U\", \"destinations\": [ {\"to\": \"Z\", \"distance\": 10}]}]}'
Write-Output ""
Write-Output "SHORTEST PATH FROM Z to U"
stack exec -- sd --shortest '{\"start\": \"Z\", \"end\": \"U\"}'
Write-Output ""
Write-Output "ADD NEW DESTINATION H to C in database"
stack exec -- sd --addroad '{\"map\":[{\"place\":\"H\", \"destinations\": [ {\"to\": \"C\", \"distance\": 2024.2}]}]}'
Write-Output ""
Write-Output "CHECK DISTANCE BETWEEN H and C"
stack exec -- sd --shortest '{\"start\": \"H\", \"end\": \"C\"}'
Write-Output ""
Write-Output "CHECK DISTANCE BETWEEN C and H"
stack exec -- sd --shortest '{\"start\": \"C\", \"end\": \"H\"}'
Write-Output ""
Write-Output "CHECK DISTANCE BETWEEN A and C again"
stack exec -- sd --shortest '{\"start\": \"A\", \"end\": \"C\"}'
Write-Output ""
Write-Output "CLEAR THE PERSISTENT DATABASE"
stack exec -- sd --clear
