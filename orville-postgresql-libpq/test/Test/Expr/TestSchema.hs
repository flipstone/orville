module Test.Expr.TestSchema
  ( FooBar (..),
    fooBarTable,
    fooColumn,
    barColumn,
    encodeFooBar,
    orderByFoo,
    insertFooBarSource,
    dropAndRecreateTestTable,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Text as T

import qualified Database.Orville.PostgreSQL.Connection as Connection
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

data FooBar = FooBar
  { foo :: Int.Int32
  , bar :: String
  }

fooBarTable :: Expr.TableName
fooBarTable =
  Expr.rawTableName "foobar"

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.rawColumnName "foo"

barColumn :: Expr.ColumnName
barColumn =
  Expr.rawColumnName "bar"

orderByFoo :: Expr.OrderByClause
orderByFoo =
  Expr.orderByClause $
    Expr.orderByExpr
      (RawSql.toRawSql fooColumn)
      Expr.ascendingOrder

encodeFooBar :: FooBar -> [(Maybe B8.ByteString, SqlValue.SqlValue)]
encodeFooBar fooBar =
  [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo fooBar))
  , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar fooBar))
  ]

insertFooBarSource :: [FooBar] -> Expr.InsertSource
insertFooBarSource fooBars =
  let mkRow fooBar =
        [ SqlValue.fromInt32 (foo fooBar)
        , SqlValue.fromText (T.pack $ bar fooBar)
        ]
   in Expr.insertSqlValues (map mkRow fooBars)

dropAndRecreateTestTable :: Connection.Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql fooBarTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> RawSql.toRawSql fooBarTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")
