{-# LANGUAGE DeriveGeneric #-}

module Neighbour ( Neighbour(..), Neighbours )
where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Neighbour = Neighbour { neighbourName :: Text, howFar :: Double } deriving (Eq, Ord, Show, Generic)
instance ToJSON Neighbour
instance FromJSON Neighbour

type Neighbours = [Neighbour]

