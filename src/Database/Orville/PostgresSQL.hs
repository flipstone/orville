{-|
Module    : Database.Orville.PostgresSQL
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

module Database.Orville.PostgresSQL
  ( createConnectionPool
  , Pool
  , Connection
  ) where

import            Data.Pool
import            Data.Time
import            Database.HDBC
import            Database.HDBC.PostgreSQL

{-|
 'createConnectionPool' allocates a pool of connections to a PosgreSQL
 server. The returned pool can be used as the endpoint to
 'Database.Orville.Core.newOrvilleEnv'
 to construct.
-}
createConnectionPool :: Int -- ^ Number of stripes in the connection pool
                     -> NominalDiffTime -- ^ Linger time before closing an idle connection
                     -> Int -- ^ Max number of connections to allocate per stripe
                     -> String -- ^ A PostgreSQL connection string
                     -> IO (Pool Connection)
createConnectionPool stripes linger maxRes connString =
  createPool (connectPostgreSQL' connString)
             disconnect
             stripes
             linger
             maxRes
