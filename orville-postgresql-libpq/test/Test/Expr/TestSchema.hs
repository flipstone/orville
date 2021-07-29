module Test.Expr.TestSchema
  ( FooBar (..),
    fooBarTable,
    fooColumn,
    barColumn,
    encodeFooBar,
    orderByFoo,
    insertFooBarSource,
    dropAndRecreateTestTable,
    assertEqualSqlRows,
    sqlRowsToText,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

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

-- SqlValue doesn't have Show or Eq, so use this to compare them in tests
assertEqualSqlRows ::
  (Show a, Eq a, HH.MonadTest m) =>
  [[(a, SqlValue.SqlValue)]] ->
  [[(a, SqlValue.SqlValue)]] ->
  m ()
assertEqualSqlRows l r = sqlRowsToText l HH.=== sqlRowsToText r

sqlRowsToText :: [[(a, SqlValue.SqlValue)]] -> [[(a, Maybe T.Text)]]
sqlRowsToText = fmap (fmap (\(a, b) -> (a, SqlValue.toText b)))
