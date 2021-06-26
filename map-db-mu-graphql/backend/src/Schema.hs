{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

import qualified Data.Text as T
import Database.Persist.Sqlite (BackendKey (SqlBackendKey))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Mu.GraphQL.Quasi (graphql)
import Mu.Schema (FromSchema)

graphql "Maps" "./map-db-mu-graphql/maps.graphql"

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Map json
  name T.Text
  UniqueName name
  deriving Show Generic
Route json
  end1 T.Text
  end2 T.Text
  deriving Show Generic
|]

newtype NewMap = NewMap
  { name :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema MapsSchema "NewMap")

data NewRoute = NewRoute
  { end1 :: T.Text,
    end2 :: T.Text,
    mapId :: Integer
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromSchema MapsSchema "NewRoute")
