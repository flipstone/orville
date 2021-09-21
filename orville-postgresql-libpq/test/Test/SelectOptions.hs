module Test.SelectOptions
  ( selectOptionsTests,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SelectOptions as SO
import qualified Test.Property as Property

selectOptionsTests :: Property.Group
selectOptionsTests =
  Property.group
    "SelectOptions"
    [ prop_emptyWhereClause
    , prop_fieldEquals
    , prop_fieldNotEquals
    , prop_fieldLessThan
    , prop_fieldGreaterThan
    , prop_fieldLessThanOrEqualTo
    , prop_fieldGreaterThanOrEqualTo
    , prop_whereAnd
    , prop_whereOr
    , prop_whereCombined
    , prop_fieldIn
    , prop_fieldNotIn
    , prop_distinct
    , prop_orderBy
    , prop_orderByCombined
    , prop_groupBy
    , prop_groupByCombined
    ]

prop_emptyWhereClause :: Property.NamedProperty
prop_emptyWhereClause =
  Property.singletonNamedProperty "emptySelectOptions yields no whereClause" $
    assertWhereClauseEquals
      Nothing
      SO.emptySelectOptions

prop_fieldEquals :: Property.NamedProperty
prop_fieldEquals =
  Property.singletonNamedProperty "fieldEquals generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1)")
      (SO.where_ $ SO.fieldEquals fooField 0)

prop_fieldNotEquals :: Property.NamedProperty
prop_fieldNotEquals =
  Property.singletonNamedProperty "fieldNotEquals generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" <> $1)")
      (SO.where_ $ SO.fieldNotEquals fooField 0)

prop_fieldLessThan :: Property.NamedProperty
prop_fieldLessThan =
  Property.singletonNamedProperty "fieldLessThan generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" < $1)")
      (SO.where_ $ SO.fieldLessThan fooField 0)

prop_fieldGreaterThan :: Property.NamedProperty
prop_fieldGreaterThan =
  Property.singletonNamedProperty "fieldGreaterThan generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" > $1)")
      (SO.where_ $ SO.fieldGreaterThan fooField 0)

prop_fieldLessThanOrEqualTo :: Property.NamedProperty
prop_fieldLessThanOrEqualTo =
  Property.singletonNamedProperty "fieldLessThanOrEqualTo generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" <= $1)")
      (SO.where_ $ SO.fieldLessThanOrEqualTo fooField 0)

prop_fieldGreaterThanOrEqualTo :: Property.NamedProperty
prop_fieldGreaterThanOrEqualTo =
  Property.singletonNamedProperty "fieldGreaterThanOrEqualTo generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" >= $1)")
      (SO.where_ $ SO.fieldGreaterThanOrEqualTo fooField 0)

prop_whereAnd :: Property.NamedProperty
prop_whereAnd =
  Property.singletonNamedProperty "whereAnd generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\" = $1) AND (\"bar\" = $2))")
      (SO.where_ $ SO.whereAnd (SO.fieldEquals fooField 10 :| [SO.fieldEquals barField 20]))

prop_whereOr :: Property.NamedProperty
prop_whereOr =
  Property.singletonNamedProperty "whereOr generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\" = $1) OR (\"bar\" = $2))")
      (SO.where_ $ SO.whereOr (SO.fieldEquals fooField 10 :| [SO.fieldEquals barField 20]))

prop_whereCombined :: Property.NamedProperty
prop_whereCombined =
  Property.singletonNamedProperty "combining SelectOptions ANDs the where clauses together" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1) AND (\"bar\" = $2)")
      ( SO.where_ (SO.fieldEquals fooField 10)
          <> SO.where_ (SO.fieldEquals barField 20)
      )

prop_fieldIn :: Property.NamedProperty
prop_fieldIn =
  Property.singletonNamedProperty "fieldIn generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" IN ($1))")
      (SO.where_ $ SO.fieldIn fooField (10 :| []))

prop_fieldNotIn :: Property.NamedProperty
prop_fieldNotIn =
  Property.singletonNamedProperty "fieldNotIn generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" NOT IN ($1, $2))")
      (SO.where_ $ SO.fieldNotIn fooField (10 :| [20]))

prop_distinct :: Property.NamedProperty
prop_distinct =
  Property.singletonNamedProperty "distinct generates expected sql" $
    assertDistinctEquals
      ("SELECT DISTINCT ")
      (SO.distinct)

prop_orderBy :: Property.NamedProperty
prop_orderBy =
  Property.singletonNamedProperty "orderBy generates expected sql" $
    assertOrderByClauseEquals
      (Just "ORDER BY \"foo\" ASC, \"bar\" DESC")
      ( SO.orderBy . Expr.orderByColumnsExpr $
          (FieldDef.fieldColumnName fooField, Expr.ascendingOrder)
            :| [(FieldDef.fieldColumnName barField, Expr.descendingOrder)]
      )

prop_orderByCombined :: Property.NamedProperty
prop_orderByCombined =
  Property.singletonNamedProperty "orderBy generates expected sql with multiple selectOptions" $
    assertOrderByClauseEquals
      (Just "ORDER BY foo ASC, \"bar\" DESC")
      ( (SO.orderBy $ Expr.orderByExpr (RawSql.fromString "foo") Expr.ascendingOrder)
          <> (SO.orderBy $ Expr.orderByExpr (RawSql.toRawSql $ FieldDef.fieldColumnName barField) Expr.descendingOrder)
      )

prop_groupBy :: Property.NamedProperty
prop_groupBy =
  Property.singletonNamedProperty "groupBy generates expected sql" $
    assertGroupByClauseEquals
      (Just "GROUP BY \"foo\", \"bar\"")
      ( SO.groupBy . Expr.groupByColumnsExpr $
          FieldDef.fieldColumnName fooField :| [FieldDef.fieldColumnName barField]
      )

prop_groupByCombined :: Property.NamedProperty
prop_groupByCombined =
  Property.singletonNamedProperty "groupBy generates expected sql with multiple selectOptions" $
    assertGroupByClauseEquals
      (Just "GROUP BY foo, \"bar\"")
      ( (SO.groupBy . Expr.groupByExpr $ RawSql.fromString "foo")
          <> (SO.groupBy . Expr.groupByExpr . RawSql.toRawSql $ FieldDef.fieldColumnName barField)
      )

assertDistinctEquals :: HH.MonadTest m => String -> SO.SelectOptions -> m ()
assertDistinctEquals mbDistinct selectOptions =
  RawSql.toBytes (SO.selectDistinct selectOptions) HH.=== B8.pack mbDistinct

assertWhereClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertWhereClauseEquals mbWhereClause selectOptions =
  fmap RawSql.toBytes (SO.selectWhereClause selectOptions) HH.=== fmap B8.pack mbWhereClause

assertOrderByClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertOrderByClauseEquals mbOrderByClause selectOptions =
  fmap RawSql.toBytes (SO.selectOrderByClause selectOptions) HH.=== fmap B8.pack mbOrderByClause

assertGroupByClauseEquals :: HH.MonadTest m => Maybe String -> SO.SelectOptions -> m ()
assertGroupByClauseEquals mbGroupByClause selectOptions =
  fmap RawSql.toBytes (SO.selectGroupByClause selectOptions) HH.=== fmap B8.pack mbGroupByClause

fooField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
fooField =
  FieldDef.integerField "foo"

barField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
barField =
  FieldDef.integerField "bar"
