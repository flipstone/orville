module Main
  ( main
  , recheckDBProperty
  )
where

import qualified Hedgehog as HH
import qualified System.Environment as Env
import qualified Test.Tasty as Tasty

import qualified Orville.PostgreSQL as Orville
import qualified Test.TypeId as TypeId

main :: IO ()
main = do
  pool <- createTestConnectionPool

  Tasty.defaultMain $
    Tasty.testGroup
      "orville-postgresql-typeid"
      [ TypeId.typeIdTests pool
      ]

createTestConnectionPool :: IO Orville.ConnectionPool
createTestConnectionPool = do
  connStr <- lookupConnStr
  -- Some tests use more than one connection, so the pool size must be greater
  -- than 1
  Orville.createConnectionPool $
    Orville.ConnectionOptions
      { Orville.connectionString = connStr
      , Orville.connectionNoticeReporting = Orville.DisableNoticeReporting
      , Orville.connectionPoolStripes = Orville.OneStripePerCapability
      , Orville.connectionPoolLingerTime = 10
      , Orville.connectionPoolMaxConnections = Orville.MaxConnectionsPerStripe 2
      }

recheckDBProperty :: HH.Size -> HH.Seed -> (Orville.ConnectionPool -> HH.Property) -> IO ()
recheckDBProperty size seed mkProperty = do
  pool <- createTestConnectionPool
  HH.recheck size seed (mkProperty pool)

lookupConnStr :: IO String
lookupConnStr = do
  mbConnHostStr <- Env.lookupEnv "TEST_CONN_HOST"
  let
    connStrUserPass = " user=orville_test password=orville"
  case mbConnHostStr of
    Nothing -> fail "TEST_CONN_HOST not set, so we don't know what database to connect to!"
    Just connHost -> pure $ connHost <> connStrUserPass
