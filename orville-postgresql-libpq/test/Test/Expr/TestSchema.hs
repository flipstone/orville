module Test.Expr.TestSchema
  ( FooBar (..),
    mkFooBar,
    findAllFooBars,
    fooBarTable,
    fooColumn,
    fooColumnRef,
    barColumn,
    barColumnRef,
    encodeFooBar,
    orderByFoo,
    insertFooBarSource,
    withFooBarData,
    dropAndRecreateTestTable,
    assertEqualFooBarRows,
    assertEqualSqlRows,
    sqlRowsToText,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Data.Text as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

data FooBar = FooBar
  { foo :: Maybe Int.Int32
  , bar :: Maybe String
  }

-- Smart constructor for the common case when both fields are not null
mkFooBar :: Int.Int32 -> String -> FooBar
mkFooBar f b = FooBar (Just f) (Just b)

fooBarTable :: Expr.Qualified Expr.TableName
fooBarTable =
  Expr.qualified Nothing (Expr.tableName "foobar")

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.columnName "foo"

fooColumnRef :: Expr.ValueExpression
fooColumnRef =
  Expr.columnReference fooColumn

barColumn :: Expr.ColumnName
barColumn =
  Expr.columnName "bar"

barColumnRef :: Expr.ValueExpression
barColumnRef =
  Expr.columnReference barColumn

orderByFoo :: Expr.OrderByClause
orderByFoo =
  Expr.orderByClause $
    Expr.orderByExpr
      (RawSql.toRawSql fooColumn)
      Expr.ascendingOrder

findAllFooBars :: Expr.QueryExpr
findAllFooBars =
  Expr.queryExpr
    (Expr.selectClause $ Expr.selectExpr Nothing)
    (Expr.selectColumns [fooColumn, barColumn])
    (Just $ Expr.tableExpr (Expr.referencesTable fooBarTable) Nothing Nothing (Just orderByFoo) Nothing Nothing)

encodeFooBar :: FooBar -> [(Maybe B8.ByteString, SqlValue.SqlValue)]
encodeFooBar fooBar =
  [ (Just (B8.pack "foo"), nullOr SqlValue.fromInt32 (foo fooBar))
  , (Just (B8.pack "bar"), nullOr SqlValue.fromText (T.pack <$> bar fooBar))
  ]

insertFooBarSource :: [FooBar] -> Expr.InsertSource
insertFooBarSource fooBars =
  let mkRow fooBar =
        [ nullOr SqlValue.fromInt32 (foo fooBar)
        , nullOr SqlValue.fromText (T.pack <$> bar fooBar)
        ]
   in Expr.insertSqlValues (map mkRow fooBars)

nullOr :: (a -> SqlValue.SqlValue) -> Maybe a -> SqlValue.SqlValue
nullOr = maybe SqlValue.sqlNull

withFooBarData ::
  Pool.Pool Orville.Connection ->
  [FooBar] ->
  (Orville.Connection -> IO a) ->
  HH.PropertyT IO a
withFooBarData pool fooBars action =
  MIO.liftIO $
    Pool.withResource pool $ \connection -> do
      dropAndRecreateTestTable connection

      RawSql.executeVoid connection $
        Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing

      action connection

dropAndRecreateTestTable :: Orville.Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql fooBarTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> RawSql.toRawSql fooBarTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")

assertEqualFooBarRows ::
  (HH.MonadTest m, HasCallStack) =>
  [[(Maybe B8.ByteString, SqlValue.SqlValue)]] ->
  [FooBar] ->
  m ()
assertEqualFooBarRows rows fooBars =
  withFrozenCallStack $
    assertEqualSqlRows rows (map encodeFooBar fooBars)

-- SqlValue doesn't have Show or Eq, so use this to compare them in tests
assertEqualSqlRows ::
  (Show a, Eq a, HH.MonadTest m, HasCallStack) =>
  [[(a, SqlValue.SqlValue)]] ->
  [[(a, SqlValue.SqlValue)]] ->
  m ()
assertEqualSqlRows l r =
  withFrozenCallStack $
    sqlRowsToText l === sqlRowsToText r

sqlRowsToText :: [[(a, SqlValue.SqlValue)]] -> [[(a, Either String T.Text)]]
sqlRowsToText = fmap (fmap (\(a, b) -> (a, SqlValue.toText b)))
