echo "--------------------------------------------------------------------"
echo ""
echo "EXAMPLE USAGE OF sd.exe"
echo "--------------------------------------------------------------------"
echo ""
echo "BUILDING sd.exe and RUNNING TESTS"
stack test

echo "--------------------------------------------------------------------"
echo ""
echo "SHOW sd.exe USAGE"
stack exec -- sd --help

echo "--------------------------------------------------------------------"
echo ""
echo "INITIALISE PERSISTENT MAP USING DATA PROVIDED WITH TEST PROBLEM"
stack exec -- sd --initialise ./test/testmap2.json

echo "--------------------------------------------------------------------"
echo ""
echo "SHORTEST PATH FROM A to B - see tests above for more comprehensive testing"
stack exec -- sd --shortest '{"start": "A", "end": "B"}'
echo ""
echo "SHORTEST PATH FROM A to H"
stack exec -- sd --shortest '{"start": "A", "end": "H"}'
echo ""
echo "SHORTEST PATH FROM A to C"
stack exec -- sd --shortest '{"start": "A", "end": "C"}'
echo ""
echo "SHORTEST PATH FROM H to B"
stack exec -- sd --shortest '{"start": "H", "end": "B"}'
echo ""
echo "SHORTEST PATH FROM H to C"
stack exec -- sd --shortest '{"start": "H", "end": "C"}'
echo ""
echo "ADD destinations I and C to place H"
stack exec -- sd --addplace '{"map":[{"place":"H", "directConnections": [ {"at": "I", "howFar": 100}, {"at": "C", "howFar": 24}]}]}'
echo ""
echo "ADD new place U with destinations V and X"
stack exec -- sd --addplace '{"map":[{"place":"U", "directConnections": [ {"at": "V", "howFar": 100}, {"at": "X", "howFar": 32.5}]}]}'
echo ""
echo "SHORTEST PATH FROM H to I"
stack exec -- sd --shortest '{"start": "H", "end": "I"}'
echo ""
echo "SHORTEST PATH FROM A to G"
stack exec -- sd --shortest '{"start": "A", "end": "G"}'
echo ""
echo "DELETE PLACE H AND ROADS TO ITS NEIGHBOURS - ALTERS DISTANCE BETWEEN A and G."
stack exec -- sd --xplace '{"map":[{"place":"H", "directConnections": []}]}'
echo ""
echo "SHORTEST PATH FROM A to G"
stack exec -- sd --shortest '{"start": "A", "end": "G"}'
echo ""
echo "SHORTEST PATH FROM A to C"
stack exec -- sd --shortest '{"start": "A", "end": "C"}'
echo ""
echo "ADD road Z to U"
stack exec -- sd --addroad '{"map":[{"place":"U", "directConnections": [ {"at": "Z", "howFar": 99.9999}]}]}'
echo ""
echo "SHORTEST PATH FROM Z to U"
stack exec -- sd --shortest '{"start": "Z", "end": "U"}'
echo ""
echo "DELIBERATE FAILURE NEXT - THERE IS NO CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{"start": "Z", "end": "A"}'
echo ""
echo "ADD connection between U and C to allow A and Z to connect."
stack exec -- sd --addroad '{"map":[{"place":"U", "directConnections": [ {"at": "C", "howFar": 123}]}]}'
echo ""
echo "THERE IS NOW A CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{"start": "Z", "end": "A"}'
echo ""
echo "REMOVE connection between U and C to stop A and Z connection."
stack exec -- sd --xroad '{"map":[{"place":"U", "directConnections": [ {"at": "C", "howFar": 123}]}]}'
echo ""
echo "DELIBERATE FAILURE NEXT - THERE IS AGAIN NO CONNECTION BETWEEN Z and A"
stack exec -- sd --shortest '{"start": "Z", "end": "A"}'
echo ""
echo "UPDATE DISTANCE BETWEEN U and Z to 10"
stack exec -- sd --addroad '{"map":[{"place":"U", "directConnections": [ {"at": "Z", "howFar": 10}]}]}'
echo ""
echo "SHORTEST PATH FROM Z to U"
stack exec -- sd --shortest '{"start": "Z", "end": "U"}'
echo ""
echo "ADD NEW DESTINATION H to C in database"
stack exec -- sd --addroad '{"map":[{"place":"H", "directConnections": [ {"at": "C", "howFar": 2024.2}]}]}'
echo ""
echo "CHECK DISTANCE BETWEEN H and C"
stack exec -- sd --shortest '{"start": "H", "end": "C"}'
echo ""
echo "CHECK DISTANCE BETWEEN C and H"
stack exec -- sd --shortest '{"start": "C", "end": "H"}'
echo ""
echo "CHECK DISTANCE BETWEEN A and C again"
stack exec -- sd --shortest '{"start": "A", "end": "C"}'
echo ""
echo "CLEAR THE PERSISTENT DATABASE"
stack exec -- sd --clear
