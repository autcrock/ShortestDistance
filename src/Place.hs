{-# LANGUAGE DeriveGeneric #-}

module Place ( Place(..), Places )
where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Destination

data Place = Place {
    place :: !Text,
    isConnectedTo :: [Destination]
 } deriving (Generic, Show, Eq)
instance ToJSON Place
instance FromJSON Place

type Places = [Place]

