module Test.SelectOptions
  ( selectOptionsTree,
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Hedgehog ((===))
import qualified Hedgehog as HH
import Test.Tasty (TestTree, testGroup)

import qualified Database.Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SelectOptions as SO

import Test.PropertyHelpers (testPropertyOnce)

selectOptionsTree :: TestTree
selectOptionsTree =
  testGroup
    "SelectOptions"
    [ testPropertyOnce "emptySelectOptions yields no whereClause" . HH.property $
        assertWhereClauseEquals
          Nothing
          SO.emptySelectOptions
    , testPropertyOnce "fieldEquals generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo = $1)")
          (SO.where_ $ SO.fieldEquals fooField 0)
    , testPropertyOnce "fieldNotEquals generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo <> $1)")
          (SO.where_ $ SO.fieldNotEquals fooField 0)
    , testPropertyOnce "fieldLessThan generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo < $1)")
          (SO.where_ $ SO.fieldLessThan fooField 0)
    , testPropertyOnce "fieldGreaterThan generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo > $1)")
          (SO.where_ $ SO.fieldGreaterThan fooField 0)
    , testPropertyOnce "fieldLessThanOrEqualTo generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo <= $1)")
          (SO.where_ $ SO.fieldLessThanOrEqualTo fooField 0)
    , testPropertyOnce "fieldGreaterThanOrEqualTo generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo >= $1)")
          (SO.where_ $ SO.fieldGreaterThanOrEqualTo fooField 0)
    , testPropertyOnce "whereAnd generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE ((foo = $1) AND (bar = $2))")
          (SO.where_ $ SO.whereAnd (SO.fieldEquals fooField 10 :| [SO.fieldEquals barField 20]))
    , testPropertyOnce "whereOr generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE ((foo = $1) OR (bar = $2))")
          (SO.where_ $ SO.whereOr (SO.fieldEquals fooField 10 :| [SO.fieldEquals barField 20]))
    , testPropertyOnce "combining SelectOptions ANDs the where clauses together" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo = $1) AND (bar = $2)")
          ( SO.where_ (SO.fieldEquals fooField 10)
              <> SO.where_ (SO.fieldEquals barField 20)
          )
    ]

assertWhereClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertWhereClauseEquals mbWhereClause selectOptions =
  fmap RawSql.toBytes (SO.selectWhereClause selectOptions) === fmap B8.pack mbWhereClause

fooField :: FieldDef.FieldDefinition FieldDef.NotNull Int32
fooField =
  FieldDef.integerField "foo"

barField :: FieldDef.FieldDefinition FieldDef.NotNull Int32
barField =
  FieldDef.integerField "bar"
