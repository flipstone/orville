module Test.SqlCommenter
  ( sqlCommenterTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Data.Functor.Identity (runIdentity)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlCommenter as SqlCommenter

import Test.Expr.TestSchema
  ( assertEqualFooBarRows
  , dropAndRecreateTestTable
  , findAllFooBars
  , fooBarTable
  , insertFooBarSource
  , mkFooBar
  )
import qualified Test.Property as Property

sqlCommenterTests :: Orville.ConnectionPool -> Property.Group
sqlCommenterTests pool =
  Property.group
    "SqlCommenterAttributes"
    [ prop_sqlcommenterAttributesEscaped
    , prop_sqlCommenterInsertExpr pool
    , prop_sqlCommenterOrvilleState pool
    ]

prop_sqlcommenterAttributesEscaped :: Property.NamedProperty
prop_sqlcommenterAttributesEscaped =
  Property.singletonNamedProperty "SqlCommenterAttributes are escaped and put at end of raw sql" $ do
    let
      rawSql :: RawSql.RawSql
      rawSql =
        SqlCommenter.addSqlCommenterAttributes staticSqlCommenterAttributes $
          RawSql.fromString "SELECT * "
            <> RawSql.fromString "FROM foo "
            <> RawSql.fromString "WHERE id = 1"

      expectedBytes =
        B8.pack "SELECT * FROM foo WHERE id = 1 /*key='value',keyForEscapedValue='queryParam%3Dfoo%20bar%2Fbaz-fizz',keyWith%27InIt='valueWith%27InIt',orm='orville'*/"

      (actualBytes, actualParams) =
        runIdentity $
          RawSql.toBytesAndParams RawSql.exampleQuoting rawSql

    actualBytes HH.=== expectedBytes
    actualParams HH.=== []

prop_sqlCommenterInsertExpr :: Property.NamedDBProperty
prop_sqlCommenterInsertExpr =
  Property.singletonNamedDBProperty "sqlcommenter support does not impact ability of insertExpr inserting values" $ \pool -> do
    let
      fooBars = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "cat"]

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          let
            insertExpr = Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing Nothing
          RawSql.executeVoid connection $
            SqlCommenter.addSqlCommenterAttributes staticSqlCommenterAttributes insertExpr

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows (NE.toList fooBars)

prop_sqlCommenterOrvilleState :: Property.NamedDBProperty
prop_sqlCommenterOrvilleState =
  Property.singletonNamedDBProperty "sqlcommenter support in OrvilleState does not impact execution" $ \pool -> do
    let
      selectOne =
        RawSql.fromString "SELECT 1 as number"

    affectedRows <-
      HH.evalIO . Orville.runOrville pool $ do
        Orville.localOrvilleState
          (Orville.setSqlCommenterAttributes staticSqlCommenterAttributes)
          (Orville.executeAndReturnAffectedRows Orville.UpdateQuery selectOne)

    affectedRows === 1

staticSqlCommenterAttributes :: SqlCommenter.SqlCommenterAttributes
staticSqlCommenterAttributes =
  fmap T.pack
    . Map.mapKeys T.pack
    $ Map.fromList
      [ ("orm", "orville")
      , ("key", "value")
      , ("keyForEscapedValue", "queryParam=foo bar/baz-fizz")
      , ("keyWith'InIt", "valueWith'InIt")
      ]
