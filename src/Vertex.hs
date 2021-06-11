{-# LANGUAGE DeriveGeneric #-}

module Vertex ( Vertex(..), Vertices ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Neighbour ( Neighbours )

data Vertex = Vertex {
        accumulatedDistance :: Double, vertex :: Text, neighbours :: Neighbours
    } deriving ( Eq, Ord, Show, Generic)
instance ToJSON Vertex
instance FromJSON Vertex

type Vertices = [Vertex]
