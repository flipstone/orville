{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main
  ( main
  ) where

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Text as T
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration

newtype Application a =
  Application (Reader.ReaderT O.OrvilleState IO a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MIO.MonadIO
    , O.MonadOrville
    , O.MonadOrvilleControl
    , O.HasOrvilleState
    )

runApplication :: O.ConnectionPool -> Application a -> IO a
runApplication pool (Application reader) =
  let
    orvilleState =
      O.newOrvilleState
        O.defaultErrorDetailLevel
        pool
  in
    Reader.runReaderT reader orvilleState

messagesTable :: O.TableDefinition O.NoKey T.Text T.Text
messagesTable =
  O.mkTableDefinitionWithoutKey
    "messages"
    (O.marshallField id (O.unboundedTextField "message"))

myApplication :: Application ()
myApplication = do
  O.insertEntity messagesTable (T.pack "Hello Orville")
  messages <- O.findEntitiesBy messagesTable mempty
  MIO.liftIO . print $ messages

schema :: [AutoMigration.SchemaItem]
schema =
  [ AutoMigration.SchemaTable messagesTable
  ]

main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=localhost user=postgres password=postgres"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }

  runApplication pool (AutoMigration.autoMigrateSchema AutoMigration.defaultOptions schema)
  runApplication pool myApplication
