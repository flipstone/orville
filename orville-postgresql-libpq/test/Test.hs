module Test
  ( runVisualTests
  ) where

import Data.Pool (Pool)
import Data.Foldable (traverse_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Database.Orville.PostgreSQL.Connection (Connection, createConnectionPool, executeRawVoid, executeRaw)
import Database.Orville.PostgreSQL.Internal.ExecutionResult (decodeRows)
import Database.Orville.PostgreSQL.Internal.SqlType (SqlType
                                                      -- numeric types
                                                    , integer
                                                    , serial
                                                    , bigInteger
                                                    , bigserial
                                                    , double

                                                    -- textual-ish types
                                                    , boolean
                                                    , unboundedText
                                                    , fixedText
                                                    , boundedText
                                                    , textSearchVector

                                                    -- date types
                                                    , date
                                                    , timestamp
                                                    )
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr

{- The following are just the beginnings of tests that for now at least let us inspect manually
  if the sqltypes are in fact
-}
runVisualTests :: String -> String -> IO ()
runVisualTests user pass = do
  let connBStr = B8.pack $ "host=localhost user=" <> user <> " password=" <> pass
  pool <- createConnectionPool 1 10 1 connBStr

-- integer
  visualTest
    pool
    integer
    "myinteger"
    "INTEGER"
    -- Note that "21474836470" and "-21474836480" are outside the acceptable range and shouldn't be reported
    ["0", "2147483647", "21474836470","-2147483648", "-21474836480"]

-- serial
  visualTest
    pool
    serial
    "myserial"
    "SERIAL"
    -- Note that "21474836470" and "-21474836480" are outside the acceptable range and shouldn't be reported
    ["0", "2147483647", "21474836470","-2147483648", "-21474836480"]

-- bigInteger
  visualTest
    pool
    bigInteger
    "mybiginteger"
    "BIGINT"
    ["0", "21474836470"]

-- bigserial
  visualTest
    pool
    bigserial
    "mybigserial"
    "BIGSERIAL"
    ["0", "21474836470"]

-- DOUBLE
  visualTest
    pool
    double
    "mydouble"
    "DOUBLE PRECISION"
    ["0.0", "1.5"]

-- BOOL
  visualTest
    pool
    boolean
    "mybool"
    "BOOL"
    ["'f'", "'t'"]

-- TEXT
  visualTest
    pool
    unboundedText
    "myunboundedtext"
    "TEXT"
    [ "'abcde'", "'THIS IS A REALLY LONG STRING OF SHORT DEFINITION REALLY LONGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA'"]

-- CHAR
  visualTest
    pool
    (fixedText 5)
    "myfixedtext"
    "CHAR(5)"
    [ "'abcde'", "'fghi'"]

-- VARCHAR
  visualTest
    pool
    (boundedText 5)
    "myboundedtext"
    "VARCHAR(5)"
    [ "'abcde'", "'fghi'"]

-- TSVector
  visualTest
    pool
    (textSearchVector)
    "mytsvector"
    "TSVECTOR"
    [ "'abcde'", "'fghi'"]

-- Date
  visualTest
    pool
    date
    "mydate"
    "DATE"
    [ "'2020-12-21 14:30:32-00'", "'2020-12-21 14:30:32+00'" ]

-- UTCTime
  visualTest
    pool
    timestamp
    "myutctime"
    "Timestamp with time zone"
    [ "'2020-12-21 14:30:32-00'", "'2020-12-21 14:30:32+00'" ]


visualTest :: Show a => Pool Connection -> SqlType a -> String -> String -> [String] -> IO ()
visualTest pool sqlType tableName sqlTypeDDL' values = do
  putStrLn $ "Testing the insert/decode of " <> sqlTypeDDL'
  executeRawVoid pool $ B8.pack $ "create table if not exists " <> tableName <> "(foo " <> sqlTypeDDL' <> ")"
  executeRawVoid pool $ B8.pack $ "truncate table " <> tableName

  let insertBS value = Expr.insertExprToSql (Expr.InsertExpr (T.pack tableName) [T.pack value])
  let insertValue value =
        executeRawVoid pool $ insertBS value
  traverse_ insertValue values

  let selectBS = Expr.queryExprToSql (Expr.QueryExpr [T.pack "*"] $ Expr.TableExpr (T.pack tableName))
  maybeResult <- executeRaw pool selectBS

  case maybeResult of
    Nothing ->
      putStrLn $ "No result for " <> sqlTypeDDL' <> " type!"

    Just res -> do
      maybeA <- decodeRows res sqlType
      print maybeA
  putStrLn ""
