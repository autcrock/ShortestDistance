{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    shortest
) where

    import Data.Aeson
    import Data.List (sortBy, nub)
    import Data.Text (Text)
    import Data.Ord (comparing)
    import GHC.Generics hiding (from, to)
    import MapDefinitions (
        Map, Place, Destination, place, destinations, distance, map, readMap, to
        )

    data Connection = Connection { from :: Text, to :: Text, distance :: Double } deriving (Show)
    
    data Child = Child { childNodeName :: Text, childDistance :: Double} deriving (Show, Generic)
    instance ToJSON Child
    instance FromJSON Child

    data ExpandedGraphNode = ExpandedGraphNode {nodeName :: Text, children :: [Child]} deriving (Show, Generic)
    instance ToJSON ExpandedGraphNode
    instance FromJSON ExpandedGraphNode

    data ExpandedGraph = ExpandedGraph {nodes :: [ExpandedGraphNode]} deriving (Show, Generic)
    instance ToJSON ExpandedGraph
    instance FromJSON ExpandedGraph

    sortByDistance :: [Child] -> [Child]
    sortByDistance = sortBy (comparing childDistance)

    getNodeNames :: [Connection] -> [Text]
    getNodeNames = nub . Prelude.map from

    pullNode :: Text -> [Connection] -> ExpandedGraphNode
    pullNode nodeName cs =
        let
            rawNodes = filter (\x -> from x == nodeName) cs
            children = Prelude.map (\x -> Child {
                 childNodeName = Shortest.to x
                 , Shortest.childDistance = Shortest.distance x
                 }) rawNodes
        in
            ExpandedGraphNode { nodeName = nodeName, children = sortByDistance children }

    allNodes :: [Connection] -> ExpandedGraph
    allNodes cs =
        let
            nodeNames = getNodeNames cs
            nodes = [ pullNode x cs | x <- nodeNames]
        in
            ExpandedGraph {nodes = nodes}
    
    insertPlaceWD :: Place -> [Connection] -> [Connection]
    insertPlaceWD place connections =
        connections ++ expandPlace place
        
    expandPlace :: Place -> [Connection]
    expandPlace p =
        expandPlace' (place p) (destinations p) []

    expandPlace' :: Text -> [Destination] -> [Connection] -> [Connection]
    expandPlace' placeName [] connections = connections
    expandPlace' placeName [destination] connections =
         connections
         ++ [Connection {
             Shortest.from = placeName,
             Shortest.to = MapDefinitions.to destination,
             Shortest.distance = MapDefinitions.distance destination }] 
         ++ [Connection {
            Shortest.from = MapDefinitions.to destination,
            Shortest.to = placeName,
            Shortest.distance = MapDefinitions.distance destination }]
    expandPlace' placeName (d : destinations) connections =
        connections
        ++ [Connection {
            Shortest.from = placeName,
            Shortest.to = MapDefinitions.to d,
            Shortest.distance = MapDefinitions.distance d }] 
        ++ [Connection {
            Shortest.from = MapDefinitions.to d,
            Shortest.to = placeName,
            Shortest.distance = MapDefinitions.distance d }]
        ++ expandPlace' placeName destinations connections

    mapToWorkingData :: Map -> [Connection]
    mapToWorkingData map =
        let
            places = MapDefinitions.map map
        in
            if null places
                then []
                else mapToWorkingData' places []
    
    mapToWorkingData' :: [Place] -> [Connection] -> [Connection]
    mapToWorkingData' [] done = done
    mapToWorkingData' [place] done  = insertPlaceWD place done
    mapToWorkingData' (place : places) done =
        mapToWorkingData' [place] done ++ mapToWorkingData' places done

    mapToExpandedGraph :: Map -> ExpandedGraph
    mapToExpandedGraph m =
        let
            wd = mapToWorkingData m
        in
            allNodes wd

    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            print (shortest' m from to)
            print (mapToExpandedGraph m)

    shortest' :: Map -> String -> String -> [Connection]
    shortest' map from to = 
        let
            workingData = mapToWorkingData map
        in
            workingData

    -- t1 =
    --     {"map":[{"destinations":[{"distance":100,"to":"B"},{"distance":30,"to":"C"}],"place":"A"},{"destinations":[{"distance":300,"to":"F"}],"place":"B"},{"destinations":[{"distance":200,"to":"D"}],"place":"C"},{"destinations":[{"distance":90,"to":"H"},{"distance":80,"to":"E"}],"place":"D"},{"destinations":[{"distance":30,"to":"H"},{"distance":150,"to":"G"},{"distance":50,"to":"F"}],"place":"E"},{"destinations":[{"distance":70,"to":"G"}],"place":"F"},{"destinations":[{"distance":50,"to":"H"}],"place":"G"}]}

    -- t2 =
    --     [Connection {from = "A", to = "B", distance = 100.0}
    --     ,Connection {from = "B", to = "A", distance = 100.0}
    --     ,Connection {from = "A", to = "C", distance = 30.0}
    --     ,Connection {from = "C", to = "A", distance = 30.0}
    --     ,Connection {from = "B", to = "F", distance = 300.0}
    --     ,Connection {from = "F", to = "B", distance = 300.0}
    --     ,Connection {from = "C", to = "D", distance = 200.0}
    --     ,Connection {from = "D", to = "C", distance = 200.0}
    --     ,Connection {from = "D", to = "H", distance = 90.0}

    --     ,Connection {from = "H", to = "D", distance = 90.0}
    --     ,Connection {from = "D", to = "E", distance = 80.0}
    --     ,Connection {from = "E", to = "D", distance = 80.0}
    --     ,Connection {from = "E", to = "H", distance = 30.0}
    --     ,Connection {from = "H", to = "E", distance = 30.0}
    --     ,Connection {from = "E", to = "G", distance = 150.0}
    --     ,Connection {from = "G", to = "E", distance = 150.0}
    --     ,Connection {from = "E", to = "F", distance = 50.0}
    --     ,Connection {from = "F", to = "E", distance = 50.0}
    --     ,Connection {from = "F", to = "G", distance = 70.0}
    --     ,Connection {from = "G", to = "F", distance = 70.0}
    --     ,Connection {from = "G", to = "H", distance = 50.0}
    --     ,Connection {from = "H", to = "G", distance = 50.0}]

    -- test = 
    --     ExpandedGraph {nodes = [
    --         ExpandedGraphNode {nodeName = "A", children = [Child {childNodeName = "C", childDistance = 30.0},Child {childNodeName = "B", childDistance = 100.0}]}
    --         ,ExpandedGraphNode {nodeName = "B", children = [Child {childNodeName = "A", childDistance = 100.0},Child {childNodeName = "F", childDistance = 300.0}]}
    --         ,ExpandedGraphNode {nodeName = "A", children = [Child {childNodeName = "C", childDistance = 30.0},Child {childNodeName = "B", childDistance = 100.0}]},
    --         ExpandedGraphNode {nodeName = "C", children = [Child {childNodeName = "A", childDistance = 30.0},Child {childNodeName = "D", childDistance = 200.0}]},
    --         ExpandedGraphNode {nodeName = "B", children = [Child {childNodeName = "A", childDistance = 100.0},Child {childNodeName = "F", childDistance = 300.0}]},
    --         ExpandedGraphNode {nodeName = "F", children = [Child {childNodeName = "E", childDistance = 50.0},Child {childNodeName = "G", childDistance = 70.0},Child {childNodeName = "B", childDistance = 300.0}]},
    --         ExpandedGraphNode {nodeName = "C", children = [Child {childNodeName = "A", childDistance = 30.0},Child {childNodeName = "D", childDistance = 200.0}]},
    --         ExpandedGraphNode {nodeName = "D", children = [Child {childNodeName = "E", childDistance = 80.0},Child {childNodeName = "H", childDistance = 90.0},Child {childNodeName = "C", childDistance = 200.0}]},
    --         ExpandedGraphNode {nodeName = "D", children = [Child {childNodeName = "E", childDistance = 80.0},Child {childNodeName = "H", childDistance = 90.0},Child {childNodeName = "C", childDistance = 200.0}]},

    --         ExpandedGraphNode {nodeName = "H", children = [Child {childNodeName = "E", childDistance = 30.0},Child {childNodeName = "G", childDistance = 50.0},Child {childNodeName = "D", childDistance = 90.0}]},
    --         ExpandedGraphNode {nodeName = "D", children = [Child {childNodeName = "E", childDistance = 80.0},Child {childNodeName = "H", childDistance = 90.0},Child {childNodeName = "C", childDistance = 200.0}]},
    --         ExpandedGraphNode {nodeName = "E", children = [Child {childNodeName = "H", childDistance = 30.0},Child {childNodeName = "F", childDistance = 50.0},Child {childNodeName = "D", childDistance = 80.0},Child {childNodeName = "G", childDistance = 150.0}]},
    --         ExpandedGraphNode {nodeName = "E", children = [Child {childNodeName = "H", childDistance = 30.0},Child {childNodeName = "F", childDistance = 50.0},Child {childNodeName = "D", childDistance = 80.0},Child {childNodeName = "G", childDistance = 150.0}]},
    --         ExpandedGraphNode {nodeName = "H", children = [Child {childNodeName = "E", childDistance = 30.0},Child {childNodeName = "G", childDistance = 50.0},Child {childNodeName = "D", childDistance = 90.0}]},
    --         ExpandedGraphNode {nodeName = "E", children = [Child {childNodeName = "H", childDistance = 30.0},Child {childNodeName = "F", childDistance = 50.0},Child {childNodeName = "D", childDistance = 80.0},Child {childNodeName = "G", childDistance = 150.0}]},
    --         ExpandedGraphNode {nodeName = "G", children = [Child {childNodeName = "H", childDistance = 50.0},Child {childNodeName = "F", childDistance = 70.0},Child {childNodeName = "E", childDistance = 150.0}]}
    --         ,ExpandedGraphNode {nodeName = "E", children = [Child {childNodeName = "H", childDistance = 30.0},Child {childNodeName = "F", childDistance = 50.0},Child {childNodeName = "D", childDistance = 80.0},Child {childNodeName = "G", childDistance = 150.0}]}
    --         ,ExpandedGraphNode {nodeName = "F", children = [Child {childNodeName = "E", childDistance = 50.0},Child {childNodeName = "G", childDistance = 70.0},Child {childNodeName = "B", childDistance = 300.0}]},
    --         ExpandedGraphNode {nodeName = "F", children = [Child {childNodeName = "E", childDistance = 50.0},Child {childNodeName = "G", childDistance = 70.0},Child {childNodeName = "B", childDistance = 300.0}]}
    --         ,ExpandedGraphNode {nodeName = "G", children = [Child {childNodeName = "H", childDistance = 50.0},Child {childNodeName = "F", childDistance = 70.0},Child {childNodeName = "E", childDistance = 150.0}]}
    --         ,ExpandedGraphNode {nodeName = "G", children = [Child {childNodeName = "H", childDistance = 50.0},Child {childNodeName = "F", childDistance = 70.0},Child {childNodeName = "E", childDistance = 150.0}]}
    --         ,ExpandedGraphNode {nodeName = "H", children = [Child {childNodeName = "E", childDistance = 30.0},Child {childNodeName = "G", childDistance = 50.0},Child {childNodeName = "D", childDistance = 90.0}]}]}
        