module Test.Expr.TestSchema
  ( FooBar (..)
  , mkFooBar
  , findAllFooBars
  , findAllFooBarsInTable
  , fooBarTable
  , fooColumn
  , fooColumnRef
  , barColumn
  , barColumnRef
  , encodeFooBar
  , orderByFoo
  , insertFooBarSource
  , withFooBarData
  , dropAndRecreateTestTable
  , assertEqualFooBarRows
  , assertEqualSqlRows
  , sqlRowsToText
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

data FooBar = FooBar
  { foo :: Maybe Int.Int32
  , bar :: Maybe String
  }

-- Smart constructor for the common case when both fields are not null
mkFooBar :: Int.Int32 -> String -> FooBar
mkFooBar f b = FooBar (Just f) (Just b)

fooBarTable :: Expr.QualifiedOrUnqualified Expr.TableName
fooBarTable =
  Expr.unqualified (Expr.tableName "foobar")

fooColumn :: Expr.QualifiedOrUnqualified Expr.ColumnName
fooColumn =
  Expr.unqualified $ Expr.columnName "foo"

fooColumnRef :: Expr.ValueExpression
fooColumnRef =
  Expr.columnReference fooColumn

barColumn :: Expr.QualifiedOrUnqualified Expr.ColumnName
barColumn =
  Expr.unqualified $ Expr.columnName "bar"

barColumnAliased :: Expr.Qualified Expr.ColumnName
barColumnAliased =
  Expr.aliasQualifyColumn (Expr.stringToAliasExpr "b") $ Expr.columnName "bar"

barColumnRef :: Expr.ValueExpression
barColumnRef =
  Expr.columnReference barColumn

orderByFoo :: Expr.OrderByClause
orderByFoo =
  Expr.orderByClause $
    Expr.orderByColumnName fooColumn Expr.ascendingOrder

findAllFooBars :: Expr.QueryExpr
findAllFooBars =
  findAllFooBarsInTable fooBarTable

findAllFooBarsInTable :: Expr.QualifiedOrUnqualified Expr.TableName -> Expr.QueryExpr
findAllFooBarsInTable tableName =
  let
    tableRefList =
      Expr.tableReferenceList
        [ Expr.tableNameReference tableName (Just (Expr.stringToAliasExpr "b"))
        ]
  in
    Expr.queryExpr
      (Expr.selectClause $ Expr.selectExpr Nothing)
      (Expr.selectColumns [fooColumn, Expr.untrackQualified barColumnAliased])
      (Just $ Expr.tableExpr tableRefList Nothing Nothing (Just orderByFoo) Nothing Nothing Nothing Nothing Nothing)

encodeFooBar :: FooBar -> [(Maybe B8.ByteString, SqlValue.SqlValue)]
encodeFooBar fooBar =
  [ (Just (B8.pack "foo"), nullOr SqlValue.fromInt32 (foo fooBar))
  , (Just (B8.pack "bar"), nullOr SqlValue.fromText (T.pack <$> bar fooBar))
  ]

insertFooBarSource :: NE.NonEmpty FooBar -> Expr.InsertSource
insertFooBarSource fooBars =
  let
    mkRow fooBar =
      NE.fromList
        [ Expr.valueExpression $ nullOr SqlValue.fromInt32 (foo fooBar)
        , Expr.valueExpression $ nullOr SqlValue.fromText (T.pack <$> bar fooBar)
        ]
  in
    Expr.valuesExprInsertSource
      . Expr.valuesExprFromValueExpressions
      $ fmap mkRow fooBars

nullOr :: (a -> SqlValue.SqlValue) -> Maybe a -> SqlValue.SqlValue
nullOr = maybe SqlValue.sqlNull

withFooBarData ::
  Orville.ConnectionPool ->
  NE.NonEmpty FooBar ->
  (Orville.Connection -> IO a) ->
  HH.PropertyT IO a
withFooBarData pool fooBars action =
  MIO.liftIO $
    Conn.withPoolConnection pool $ \connection -> do
      dropAndRecreateTestTable connection

      RawSql.executeVoid connection $
        Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing Nothing

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
