module Test.SelectOptions
  ( selectOptionsTests,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NEL
import qualified Data.String as String
import qualified Hedgehog as HH

import qualified Database.Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SelectOptions as SO
import qualified Test.Property as Property

selectOptionsTests :: IO Bool
selectOptionsTests =
  HH.checkParallel $
    HH.Group
      (String.fromString "SelectOptions")
      [
        ( String.fromString "emptySelectOptions yields no whereClause"
        , Property.singletonProperty $
            assertWhereClauseEquals
              Nothing
              SO.emptySelectOptions
        )
      ,
        ( String.fromString "fieldEquals generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo = $1)")
              (SO.where_ $ SO.fieldEquals fooField 0)
        )
      ,
        ( String.fromString "fieldNotEquals generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo <> $1)")
              (SO.where_ $ SO.fieldNotEquals fooField 0)
        )
      ,
        ( String.fromString "fieldLessThan generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo < $1)")
              (SO.where_ $ SO.fieldLessThan fooField 0)
        )
      ,
        ( String.fromString "fieldGreaterThan generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo > $1)")
              (SO.where_ $ SO.fieldGreaterThan fooField 0)
        )
      ,
        ( String.fromString "fieldLessThanOrEqualTo generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo <= $1)")
              (SO.where_ $ SO.fieldLessThanOrEqualTo fooField 0)
        )
      ,
        ( String.fromString "fieldGreaterThanOrEqualTo generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo >= $1)")
              (SO.where_ $ SO.fieldGreaterThanOrEqualTo fooField 0)
        )
      ,
        ( String.fromString "whereAnd generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE ((foo = $1) AND (bar = $2))")
              (SO.where_ $ SO.whereAnd (SO.fieldEquals fooField 10 NEL.:| [SO.fieldEquals barField 20]))
        )
      ,
        ( String.fromString "whereOr generates expected sql"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE ((foo = $1) OR (bar = $2))")
              (SO.where_ $ SO.whereOr (SO.fieldEquals fooField 10 NEL.:| [SO.fieldEquals barField 20]))
        )
      ,
        ( String.fromString "combining SelectOptions ANDs the where clauses together"
        , Property.singletonProperty $
            assertWhereClauseEquals
              (Just "WHERE (foo = $1) AND (bar = $2)")
              ( SO.where_ (SO.fieldEquals fooField 10)
                  <> SO.where_ (SO.fieldEquals barField 20)
              )
        )
      ]

assertWhereClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertWhereClauseEquals mbWhereClause selectOptions =
  fmap RawSql.toBytes (SO.selectWhereClause selectOptions) HH.=== fmap B8.pack mbWhereClause

fooField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
fooField =
  FieldDef.integerField "foo"

barField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
barField =
  FieldDef.integerField "bar"
