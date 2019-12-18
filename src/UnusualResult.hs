{-# LANGUAGE DeriveGeneric #-}

module UnusualResult(UnusualResult(..))
where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data UnusualResult = NegativeRoadLength | NotConnected Text Text deriving (Show, Generic, Eq)
instance ToJSON UnusualResult
instance FromJSON UnusualResult

