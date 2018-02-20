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

createConnectionPool :: Int -- Stripe Count
                     -> NominalDiffTime -- Linger time
                     -> Int -- Max resources per stripe
                     -> String
                     -> IO (Pool Connection)
createConnectionPool stripes linger maxRes connString =
  createPool (connectPostgreSQL' connString)
             disconnect
             stripes
             linger
             maxRes
