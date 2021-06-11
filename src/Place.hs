{-# LANGUAGE DeriveGeneric #-}

module Place ( Place(..), Places )
where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Destination ( Destinations )

data Place = Place {
    place :: !Text,
    isConnectedTo :: Destinations
 } deriving (Generic, Show, Eq)
instance ToJSON Place
instance FromJSON Place

type Places = [Place]

