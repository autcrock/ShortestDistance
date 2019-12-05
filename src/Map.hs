{-# LANGUAGE DeriveGeneric #-}

module Map ( Map(..) ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Place

newtype Map = Map{map :: Places} deriving (Generic, Show, Eq)
instance ToJSON Map
instance FromJSON Map
