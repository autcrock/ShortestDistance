{-# LANGUAGE DeriveGeneric #-}

module Graph ( Graph (..) ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Vertex ( Vertices )

newtype Graph = Graph{vertices :: Vertices} deriving (Show, Generic)
instance ToJSON Graph
instance FromJSON Graph
