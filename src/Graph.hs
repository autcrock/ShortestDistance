{-# LANGUAGE DeriveGeneric #-}

module Graph ( Graph (..) ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Vertex

newtype Graph = Graph{vertices :: Vertices} deriving (Show, Generic)
instance ToJSON Graph
instance FromJSON Graph
