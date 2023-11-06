module Main
  ( main
  ) where

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Raw.Connection as Connection
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

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

  Connection.withPoolConnection pool $ \connection -> do
    result <- RawSql.execute connection (RawSql.fromString "SELECT 1+1")
    [[(_, sqlValue)]] <- Execution.readRows result
    print (SqlValue.toInt sqlValue)
