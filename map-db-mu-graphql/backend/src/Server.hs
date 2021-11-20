{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
-- {-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfoN, runStderrLoggingT)
import Data.Conduit (ConduitT, Void, runConduit, (.|))
import Data.Conduit.Combinators (yieldMany)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Database.Persist.Sqlite
import Mu.Adapter.Persistent (runDb)
import Mu.GraphQL.Server (graphQLApp, liftServerConduit)
import Mu.Instrumentation.Prometheus (initPrometheus, prometheus)
import Mu.Schema (Mapping ((:->)), Proxy (Proxy))
import Mu.Server
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Schema

main :: IO ()
main = do
  -- Setup CORS
  let hm =
        addHeaders
          [ ("Access-Control-Allow-Origin", "*"),
            ("Access-Control-Allow-Headers", "Content-Type")
          ]
  p <- initPrometheus "map"
  runStderrLoggingT $
    withSqliteConn ":memory:" $ \conn -> do
      runDb conn $ runMigration migrateAll
      insertSeedData conn
      logInfoN "starting GraphQL server on port 8000"
      liftIO $
        run 8000 $
          hm $
            graphQLApp
              (prometheus p $ mapServer conn)
              (Proxy @('Just "Query"))
              (Proxy @('Just "Mutation"))
              (Proxy @('Just "Subscription"))

-- | Inserts demo data to make this example valueable for testing with different clients
--   Returns Nothing in case of any failure, including attempts to insert non-unique values
insertSeedData :: SqlBackend -> LoggingT IO (Maybe ())
insertSeedData conn =
  sequence_
    <$> traverse
      (uncurry $ insertMapAndRoutes conn)
      [ ( Map "Test One",
          [ Route "Route 1" "Route 2",
            Route "Route 2" "Route3",
            Route "Route 3" "Route4",
            Route "Route 3" "Route1"
          ]
        ),
        ( Map "Test Two",
          [ Route "Route 1" "Route 5",
            Route "Route 5" "Route6",
            Route "Route 6" "Route7",
            Route "Route 7" "Route3"
          ]
        )
      ]

-- | Inserts Map and Routes
--   Returns Nothing in case of any failure, including attempts to insert non-unique values
insertMapAndRoutes :: SqlBackend -> Map -> [Key Map -> Route] -> LoggingT IO (Maybe ())
insertMapAndRoutes conn map routes =
  runDb conn . fmap sequence_ $ do
    mapResult <- insertUnique map
    case mapResult of
      Just mapGuid -> traverse (\kroute -> insertUnique (kroute mapGuid)) routes
      Nothing -> pure [Nothing]

type ObjectMapping =
  '[ "Route" ':-> Entity Route,
     "Map" ':-> Entity Map
   ]

mapServer :: SqlBackend -> ServerT ObjectMapping i Map ServerErrorIO _
mapServer conn =
  resolver
    ( object @"Route"
        ( field @"id" routeGuid,
          field @"end1" end1,
          field @"end2" end2
        ),
      object @"Map"
        ( field @"id" mapGuid,
          field @"name" mapName
        ),
      object @"Query"
        ( method @"maps" allMaps,
          method @"routes" allRoutes
        ),
      object @"Mutation"
        ( method @"newMap" newMap,
          method @"newRoute" newRoute
        ),
      object @"Subscription"
        (method @"allRoutes" allRoutesConduit)
    )
  where
    routeGuid :: Entity Route -> ServerErrorIO Integer
    routeGuid (Entity (RouteKey k) _) = pure $ toInteger k

    routeEnd1 :: Entity Route -> ServerErrorIO T.Text
    routeEnd1 (Entity _ Route {routeEnd1}) = pure routeEnd1

    routeEnd2 :: Entity Route -> ServerErrorIO T.Text
    routeEnd2 (Entity _ Route {routeEnd2}) = pure routeEnd2

    -- routeMap (Entity _ Route {routeMap}) = do
    --   map <- runDb conn $ get routeMap
    --   pure $ Entity routeMap (fromJust map)

    mapGuid :: Entity Map -> ServerErrorIO Integer
    mapGuid (Entity (MapKey k) _) = pure $ toInteger k

    mapName :: Entity Map -> ServerErrorIO T.Text
    mapName (Entity _ Map {mapName}) = pure mapName

    mapRoutes :: Entity Map -> ServerErrorIO [Entity Route]
    mapRoutes (Entity map _) =
      runDb conn $
        selectList [RouteMap ==. map] [Asc RouteTitle]

    allMaps :: T.Text -> ServerErrorIO [Entity Map]
    allMaps nameFilter =
      runDb conn $
        selectList
          [ Filter
              MapName
              (FilterValue nameFilter)
              (BackendSpecificFilter "LIKE")
          ]
          [Asc MapName]

    allRoutes :: T.Text -> ServerErrorIO [Entity Route]
    allRoutes end1Filter =
      runDb conn $
        selectList
          [ Filter
              RouteName
              (FilterValue end1Filter)
              (BackendSpecificFilter "LIKE")
          ]
          [Asc RouteName]

    allRoutesConduit :: ConduitT (Entity Route) Void ServerErrorIO () -> ServerErrorIO ()
    allRoutesConduit sink = do
      -- do not convert to a single selectConduit!
      -- there seems to be a problem running nested runDb's
      -- so we break it into two steps, assuming that the
      -- list of routes would fit in memory
      -- see https://github.com/higherkindness/mu-haskell/issues/259
      lst <- liftIO $ runDb conn $ selectList [] [Asc RouteEnd1]
      runConduit $ yieldMany lst .| liftServerConduit sink

    insertNewEntity :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => T.Text -> a -> ServerErrorIO (Entity a)
    insertNewEntity name new = do
      maybeEntity <- runDb conn $ do
        result <- insertUnique new
        pure $ Entity <$> result <*> pure new
      let errorMsg = T.unpack name <> "\" already exists"
      maybe (serverError $ ServerError Invalid errorMsg) pure maybeEntity

    newMap :: NewMap -> ServerErrorIO (Entity Map)
    newMap (NewMap name) = insertNewEntity ("Map " <> name) (Map name)

    newRoute :: NewRoute -> ServerErrorIO (Entity Route)
    newRoute (NewRoute title mapGuid img) = insertNewEntity ("Route " <> title) (Route title img . toSqlKey $ fromInteger mapGuid)
