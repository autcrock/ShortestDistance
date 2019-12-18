{-# LANGUAGE DeriveGeneric #-}

module Destination ( Destination(..), Destinations )

where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Destination = Destination {
    at :: !Text,
    howFar :: Double
} deriving (Generic, Show, Eq)

instance ToJSON Destination
instance FromJSON Destination

type Destinations = [Destination]