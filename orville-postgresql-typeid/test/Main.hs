module Main
  ( main
  , recheckDBProperty
  )
where

import qualified Control.Monad as Monad
import qualified Hedgehog as HH
import qualified System.Environment as Env
import qualified System.Exit as SE

import qualified Orville.PostgreSQL as Orville
import qualified Test.Property as Property
import qualified Test.TypeId as TypeId

main :: IO ()
main = do
  pool <- createTestConnectionPool

  summary <-
    Property.checkGroups
      [ TypeId.typeIdTests pool
      ]

  Monad.unless (Property.allPassed summary) SE.exitFailure

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

recheckDBProperty :: HH.Size -> HH.Seed -> Property.NamedDBProperty -> IO ()
recheckDBProperty size seed namedProperty = do
  pool <- createTestConnectionPool
  HH.recheck size seed (snd $ namedProperty pool)

lookupConnStr :: IO String
lookupConnStr = do
  mbConnHostStr <- Env.lookupEnv "TEST_CONN_HOST"
  let
    connStrUserPass = " user=orville_test password=orville"
  case mbConnHostStr of
    Nothing -> fail "TEST_CONN_HOST not set, so we don't know what database to connect to!"
    Just connHost -> pure $ connHost <> connStrUserPass
