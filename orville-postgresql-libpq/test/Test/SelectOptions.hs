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
    , testPropertyOnce "whereEquals generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo = $1)")
          (SO.where_ $ SO.whereEquals fooField 0)
    , testPropertyOnce "whereNotEquals generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo <> $1)")
          (SO.where_ $ SO.whereNotEquals fooField 0)
    , testPropertyOnce "whereLessThan generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo < $1)")
          (SO.where_ $ SO.whereLessThan fooField 0)
    , testPropertyOnce "whereGreaterThan generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo > $1)")
          (SO.where_ $ SO.whereGreaterThan fooField 0)
    , testPropertyOnce "whereLessThanOrEqualTo generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo <= $1)")
          (SO.where_ $ SO.whereLessThanOrEqualTo fooField 0)
    , testPropertyOnce "whereGreaterThanOrEqualTo generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo >= $1)")
          (SO.where_ $ SO.whereGreaterThanOrEqualTo fooField 0)
    , testPropertyOnce "whereAnd generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE ((foo = $1) AND (bar = $2))")
          (SO.where_ $ SO.whereAnd (SO.whereEquals fooField 10 :| [SO.whereEquals barField 20]))
    , testPropertyOnce "whereOr generates expected sql" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE ((foo = $1) OR (bar = $2))")
          (SO.where_ $ SO.whereOr (SO.whereEquals fooField 10 :| [SO.whereEquals barField 20]))
    , testPropertyOnce "combining SelectOptions ANDs the where clauses together" . HH.property $
        assertWhereClauseEquals
          (Just "WHERE (foo = $1) AND (bar = $2)")
          ( SO.where_ (SO.whereEquals fooField 10)
              <> SO.where_ (SO.whereEquals barField 20)
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
