{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    shortest
) where

    import Data.Aeson
    import Data.List (sortBy, nub, find, delete, deleteBy)
    import Data.Maybe (fromJust, fromMaybe, isNothing)
    import Data.Ord (comparing)
    import Data.Text (Text, pack)
    import GHC.Generics hiding (from, to)
    import MapDefinitions (
        Map, Place, Destination, place, destinations, distance, map, readMap, to
        )
    import Numeric.Natural (Natural)

    data Connection = Connection { from :: Text, to :: Text, distance :: Double } deriving (Show)
    
    data Child = Child { childNodeName :: Text, childDistance :: Double} deriving (Show, Generic)
    instance ToJSON Child
    instance FromJSON Child

    data ParametrisedGraphNode = ParametrisedGraphNode {
            pNodeName :: Text, parameter :: Double, pChildren :: [Child]
        } deriving (Show, Generic)
    instance ToJSON ParametrisedGraphNode
    instance FromJSON ParametrisedGraphNode

    data ParametrisedGraph = ParametrisedGraph {pNodes :: [ParametrisedGraphNode]} deriving (Show, Generic)
    instance ToJSON ParametrisedGraph
    instance FromJSON ParametrisedGraph

    sortByDistance :: [Child] -> [Child]
    sortByDistance = sortBy (comparing childDistance)

    getNodeNames :: [Connection] -> [Text]
    getNodeNames = nub . Prelude.map from

    pullNode :: Text -> [Connection] -> Double -> ParametrisedGraphNode
    pullNode nodeName cs parameterValue =
        let
            rawNodes = filter (\x -> from x == nodeName) cs
            children = Prelude.map (\x -> Child {
                 childNodeName = Shortest.to x
                 , Shortest.childDistance = Shortest.distance x
                 }) rawNodes
        in
            ParametrisedGraphNode {
                pNodeName = nodeName
                , parameter = parameterValue
                , pChildren = sortByDistance children
                }

    makeInfinity :: Double
    makeInfinity = read "Infinity" ::Double

    allNodes :: [Connection] -> ParametrisedGraph
    allNodes cs =
        let
            infinity = makeInfinity
            nodeNames = getNodeNames cs
            nodes = [ pullNode x cs infinity | x <- nodeNames]
        in
            ParametrisedGraph {pNodes = nodes}
    
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

    mapToParametrisedGraph :: Map -> ParametrisedGraph
    mapToParametrisedGraph m =
        let
            wd = mapToWorkingData m
        in
            allNodes wd

    parametrisedGraphGetNodeChildren :: ParametrisedGraph -> Text -> Maybe [Child]
    parametrisedGraphGetNodeChildren pg node = 
        let
            n = parametrisedGraphGetNode pg node
        in
            if isNothing n
            then
                Nothing
            else
                Just (pChildren $ fromJust n)
    
    parametrisedGraphGetClosestToNode :: ParametrisedGraph -> Text -> Maybe Child
    parametrisedGraphGetClosestToNode pg node =
        let
            n = parametrisedGraphGetNode pg node
        in
            if isNothing n
            then
                Nothing
            else
                Just (head (pChildren $ fromJust n))
    
    parametrisedGraphGetNode :: ParametrisedGraph -> Text -> Maybe ParametrisedGraphNode
    parametrisedGraphGetNode pg node = find (\x -> pNodeName x == node) (pNodes pg)
    
    parametrisedGraphGetParameter :: ParametrisedGraph -> Text -> Maybe Double
    parametrisedGraphGetParameter pg node =
        let
            n = parametrisedGraphGetNode pg node
        in
            if isNothing n
            then
                Nothing
            else
                Just (parameter $ fromJust n)
    
    parametrisedGraphDeleteNode :: ParametrisedGraph -> ParametrisedGraphNode -> ParametrisedGraph
    parametrisedGraphDeleteNode pg node =
        let 
            ns = deleteBy (\x y -> pNodeName x == pNodeName node) node (pNodes pg)
        in 
            ParametrisedGraph { pNodes = ns }
            
    parametrisedGraphInsert  :: ParametrisedGraph -> ParametrisedGraphNode -> ParametrisedGraph
    parametrisedGraphInsert pg node =
        ParametrisedGraph { pNodes = node : pNodes pg}

    transferNodeUpdatingParameter :: ParametrisedGraph -> Text -> Double -> ParametrisedGraph -> (ParametrisedGraph, ParametrisedGraph)
    transferNodeUpdatingParameter graph1 nodeName parameter_in graph2 =
        let
            txNode = parametrisedGraphGetNode graph1 nodeName
        in
            if isNothing txNode
            then (graph1, graph2)
            else
                let 
                    txN = fromJust txNode

                    parNode = ParametrisedGraphNode {
                        pNodeName = pNodeName txN
                        , parameter = parameter_in
                        , pChildren = pChildren txN
                    }
                    newGraph2 = parametrisedGraphInsert graph2 parNode
                    newGraph1 = parametrisedGraphDeleteNode graph1 txN
                in
                    (newGraph1, newGraph2) 

    transferNode :: ParametrisedGraph -> Text -> ParametrisedGraph -> (ParametrisedGraph, ParametrisedGraph)
    transferNode graph1 nodeName graph2 =
        let
            txNode = parametrisedGraphGetNode graph1 nodeName
        in
            if isNothing txNode
            then (graph1, graph2)
            else
                let 
                    txN = fromJust txNode
                    newGraph2 = parametrisedGraphInsert graph2 txN
                    newGraph1 = parametrisedGraphDeleteNode graph1 txN
                in
                    (newGraph1, newGraph2) 

    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            let pg = mapToParametrisedGraph m
            let (reds, yellows) =
                    transferNodeUpdatingParameter pg (pack from) 0.0 ParametrisedGraph{pNodes = []}
            let greens = ParametrisedGraph{pNodes = []}
            shortest' reds yellows greens (pack from) (pack to)

    shortest' :: ParametrisedGraph -> ParametrisedGraph -> ParametrisedGraph -> Text -> Text -> IO()
    shortest' reds yellows greens from to = 
        let
            v = head (pNodes yellows)
            children = pChildren v
            closest = head children
            storedDistance = childDistance closest 
        in
            if null children
            then
                error "shortest': Unexpectedly could not find children."
            else
                let
                    -- content = fromMaybe null children
                    -- closest = head content
                    -- remainder = tail content
                    closest1 = parametrisedGraphGetClosestToNode yellows from
--                    when isNothing closest $ error ("shortest': Unexpectedly found no closest node")
                    closestNodeName = childNodeName $ fromJust closest1
                    (ys1, gs1) = transferNodeUpdatingParameter yellows from storedDistance greens
                    (rs2, ys2) = transferNodeUpdatingParameter reds closestNodeName 0 yellows
                in
                    do
                        print "==================="
                        print ("reds = " ++ show reds)
                        print "==================="
                        print ("yellows = " ++ show yellows)
                        print "==================="
                        print ("greens = " ++ show greens)
                        print "==================="
                        print ("ys1 = " ++ show ys1)
                        print "==================="
                        print ("gs1 = " ++ show gs1)
                        print "==================="
                        print ("rs2 = " ++ show rs2)
                        print "==================="
                        print ("ys2 = " ++ show ys2)
                        print "==================="
