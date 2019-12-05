{-# LANGUAGE DeriveGeneric #-}

module Distance (
    Distance(..)
)
where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics hiding (to)

newtype Distance = Distance{ distance :: Double } deriving (Generic, Show, Eq)
instance ToJSON Distance
instance FromJSON Distance
