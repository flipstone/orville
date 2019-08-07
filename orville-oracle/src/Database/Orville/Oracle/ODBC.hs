{-|
Module    : Database.Orville.Oracle.ODBC
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.ODBC
  ( createConnectionPool
  , Pool
  , Connection
  ) where

import Data.Pool
import Data.Time
import Database.HDBC
import Database.HDBC.ODBC

{-|
 'createConnectionPool' allocates a pool of connections to a ODBC compliant
 server. The returned pool can be used as the endpoint to
 'Database.Orville.Oracle.Core.newOrvilleEnv'
 to construct.
-}
createConnectionPool ::
     Int -- ^ Number of stripes in the connection pool
  -> NominalDiffTime -- ^ Linger time before closing an idle connection
  -> Int -- ^ Max number of connections to allocate per stripe
  -> String -- ^ An ODBC connection string
  -> IO (Pool Connection)
createConnectionPool stripes linger maxRes connString =
  createPool (connectODBC connString) disconnect stripes linger maxRes
