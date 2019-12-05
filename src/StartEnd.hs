{-# LANGUAGE DeriveGeneric #-}

module StartEnd (
  StartEnd(..)
) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Names of a start and an end
data StartEnd = StartEnd {
    start :: !Text,
    end :: !Text
 } deriving (Generic, Show, Eq)
instance ToJSON StartEnd
instance FromJSON StartEnd
