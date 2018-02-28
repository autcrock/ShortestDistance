{-# LANGUAGE DeriveGeneric #-}

module Shortest (
    shortest
) where

    import Data.Aeson
    import Data.List (sortBy, nub, find, delete, deleteBy)
    import Data.Maybe (fromMaybe, isNothing)
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

    data ExpandedGraphNode = ExpandedGraphNode {
            egNodeName :: Text, egChildren :: [Child]
        } deriving (Show, Generic)
    instance ToJSON ExpandedGraphNode
    instance FromJSON ExpandedGraphNode

    data ExpandedGraph = ExpandedGraph {egNodes :: [ExpandedGraphNode]} deriving (Show, Generic)
    instance ToJSON ExpandedGraph
    instance FromJSON ExpandedGraph

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

    pullNode :: Text -> [Connection] -> ExpandedGraphNode
    pullNode nodeName cs =
        let
            rawNodes = filter (\x -> from x == nodeName) cs
            children = Prelude.map (\x -> Child {
                 childNodeName = Shortest.to x
                 , Shortest.childDistance = Shortest.distance x
                 }) rawNodes
        in
            ExpandedGraphNode { egNodeName = nodeName, egChildren = sortByDistance children }

    allNodes :: [Connection] -> ExpandedGraph
    allNodes cs =
        let
            nodeNames = getNodeNames cs
            nodes = [ pullNode x cs | x <- nodeNames]
        in
            ExpandedGraph {egNodes = nodes}
    
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

    expandedGraphGetNode :: ExpandedGraph -> Text -> ExpandedGraphNode
    expandedGraphGetNode eg node =
        let 
            n = find (\x -> egNodeName x == node) (egNodes eg)
        in
            fromMaybe ExpandedGraphNode {
                    egNodeName = pack "Empty", egChildren = []
                }
                n
    
    expandedGraphGetNodeChildren :: ExpandedGraph -> Text -> Maybe [Child]
    expandedGraphGetNodeChildren eg node =
        let
            n = expandedGraphGetNode eg node
        in
            if egNodeName n == pack "Empty"
            then
                Nothing
            else
                Just (egChildren n)
    
    expandedGraphGetClosestToNode :: ExpandedGraph -> Text -> Maybe Child
    expandedGraphGetClosestToNode eg node =
        let
            n = expandedGraphGetNode eg node
        in
            if egNodeName n == pack "Empty"
            then
                Nothing
            else
                head (egChildren n)
    
    parametrisedGraphGetNodeChildren :: ParametrisedGraph -> Text -> Maybe [Child]
    parametrisedGraphGetNodeChildren pg node =
        let
            n = parametrisedGraphGetNode pg node
        in
            if pNodeName n == pack "Empty"
            then
                Nothing
            else
                Just (pChildren n)
    
    parametrisedGraphGetClosestToNode :: ParametrisedGraph -> Text -> Maybe Child
    parametrisedGraphGetClosestToNode pg node =
        let
            n = parametrisedGraphGetNode pg node
        in
            if pNodeName n == pack "Empty"
            then
                Nothing
            else
                head (pChildren n)
    
    parametrisedGraphGetNode :: ParametrisedGraph -> Text -> ParametrisedGraphNode
    parametrisedGraphGetNode pg node =
        let 
            n = find (\x -> pNodeName x == node) (pNodes pg)
        in
            fromMaybe ParametrisedGraphNode {
                    pNodeName = pack "Empty", parameter = -1, pChildren = []
                }
                n
    
    parametrisedGraphGetParameter :: ParametrisedGraph -> Text -> Double
    parametrisedGraphGetParameter pg node =
        let 
            n = find (\x -> pNodeName x == node) (pNodes pg)
        in
            maybe (-1) parameter n
    
    expandedGraphDeleteNode :: ExpandedGraph -> Text -> ExpandedGraph
    expandedGraphDeleteNode eg node =
        let 
            ns = deleteBy (\x y -> egNodeName x == node)
                     ExpandedGraphNode {
                         egNodeName = pack "Empty", egChildren = []
                    }
                    (egNodes eg)
        in 
            ExpandedGraph { egNodes = ns }
            
    parametrisedGraphDeleteNode :: ParametrisedGraph -> Text -> ParametrisedGraph
    parametrisedGraphDeleteNode pg node =
        let 
            ns = deleteBy (\x y -> pNodeName x == node)
                    ParametrisedGraphNode {
                         pNodeName = pack "Empty", parameter = -1, pChildren = []
                    }
                    (pNodes pg)
        in 
            ParametrisedGraph { pNodes = ns }
            
    parametrisedGraphInsert  :: ParametrisedGraph -> ParametrisedGraphNode -> ParametrisedGraph
    parametrisedGraphInsert pg node =
        ParametrisedGraph { pNodes = node : pNodes pg}

    transferRedToYellow :: ExpandedGraph -> Text -> Double -> ParametrisedGraph -> (ExpandedGraph, ParametrisedGraph)
    transferRedToYellow reds node parameter yellows =
        let
            exNode = expandedGraphGetNode reds node
        in
            if egNodeName exNode == pack "Empty"
            then (reds, yellows)
            else
                let 
                    parNode = ParametrisedGraphNode {
                        pNodeName = egNodeName exNode
                        , parameter = parameter
                        , pChildren = egChildren exNode
                    }
                    newYellows = parametrisedGraphInsert yellows parNode
                    newReds = expandedGraphDeleteNode reds node
                in
                    (newReds, newYellows) 

    transferYellowToGreen :: ParametrisedGraph -> Text -> Double -> ParametrisedGraph -> (ParametrisedGraph, ParametrisedGraph)
    transferYellowToGreen yellows node parameter greens =
        let
            exNode = parametrisedGraphGetNode yellows node
        in
            if pNodeName exNode == pack "Empty"
            then (yellows, greens)
            else
                let 
                    parNode = ParametrisedGraphNode {
                        pNodeName = pNodeName exNode
                        , parameter = parameter
                        , pChildren = pChildren exNode
                    }
                    newGreens = parametrisedGraphInsert greens parNode
                    newYellows = parametrisedGraphDeleteNode yellows node
                in
                    (newYellows, newGreens) 

    shortest :: String -> String -> IO ()
    shortest from to =
        do 
            m <- readMap
            let eg = mapToExpandedGraph m
            let (reds, yellows) = transferRedToYellow eg (pack from) 0.0 ParametrisedGraph{pNodes = []}             
            shortest' reds yellows ParametrisedGraph{pNodes = []} (pack from) (pack to)

    shortest' :: ExpandedGraph -> ParametrisedGraph -> ParametrisedGraph -> Text -> Text -> IO()
    shortest' reds yellows greens from to = 
        let
            v = head (pNodes yellows)
            children = pChildren v
            closest = head children
            storedDistance = parameter closest 
        in
            if children == []
            then
                error "shortest': Unexpectedly could not find children."
            else
                let
                    content = fromMaybe null children
                    closest = head content
                    remainder = tail content
                    (ys,gs) = transferYellowToGreen yellows from storedDistance greens
                    (rs, ys) = transferRedToYellow reds (egNodeName closest) 
                in


 