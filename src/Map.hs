{-# LANGUAGE DeriveGeneric #-}

module Map ( Map(..) ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Place ( Places )

newtype Map = Map{map :: Places} deriving (Generic, Show, Eq)
instance ToJSON Map
instance FromJSON Map
