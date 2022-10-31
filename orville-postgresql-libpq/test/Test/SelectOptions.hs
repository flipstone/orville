module Test.SelectOptions
  ( selectOptionsTests,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SelectOptions ((.&&), (./=), (.<), (.<-), (.</-), (.<=), (.==), (.>), (.>=), (.||))
import qualified Orville.PostgreSQL.Internal.SelectOptions as SO
import qualified Test.Property as Property

selectOptionsTests :: Property.Group
selectOptionsTests =
  Property.group
    "SelectOptions"
    [ prop_emptyWhereClause
    , prop_fieldEquals
    , prop_fieldEqualsOperator
    , prop_fieldNotEquals
    , prop_fieldNotEqualsOperator
    , prop_fieldLessThan
    , prop_fieldLessThanOperator
    , prop_fieldGreaterThan
    , prop_fieldGreaterThanOperator
    , prop_fieldLessThanOrEqualTo
    , prop_fieldLessThanOrEqualToOperator
    , prop_fieldGreaterThanOrEqualTo
    , prop_fieldGreaterThanOrEqualToOperator
    , prop_fieldIsNull
    , prop_fieldIsNotNull
    , prop_fieldLike
    , prop_fieldLikeInsensitive
    , prop_whereAnd
    , prop_whereAndOperator
    , prop_whereOr
    , prop_whereOrOperator
    , prop_whereAndOrRightAssociativity
    , prop_equalityAndOrPrecedence
    , prop_whereCombined
    , prop_fieldIn
    , prop_fieldInOperator
    , prop_fieldNotIn
    , prop_fieldNotInOperator
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
      (Just "WHERE \"foo\" = $1")
      (SO.where_ $ SO.fieldEquals fooField 0)

prop_fieldEqualsOperator :: Property.NamedProperty
prop_fieldEqualsOperator =
  Property.singletonNamedProperty ".== generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" = $1")
      (SO.where_ $ fooField .== 0)

prop_fieldNotEquals :: Property.NamedProperty
prop_fieldNotEquals =
  Property.singletonNamedProperty "fieldNotEquals generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" <> $1")
      (SO.where_ $ SO.fieldNotEquals fooField 0)

prop_fieldNotEqualsOperator :: Property.NamedProperty
prop_fieldNotEqualsOperator =
  Property.singletonNamedProperty "./= generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" <> $1")
      (SO.where_ $ fooField ./= 0)

prop_fieldLessThan :: Property.NamedProperty
prop_fieldLessThan =
  Property.singletonNamedProperty "fieldLessThan generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" < $1")
      (SO.where_ $ SO.fieldLessThan fooField 0)

prop_fieldLessThanOperator :: Property.NamedProperty
prop_fieldLessThanOperator =
  Property.singletonNamedProperty ".< generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" < $1")
      (SO.where_ $ fooField .< 0)

prop_fieldGreaterThan :: Property.NamedProperty
prop_fieldGreaterThan =
  Property.singletonNamedProperty "fieldGreaterThan generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" > $1")
      (SO.where_ $ SO.fieldGreaterThan fooField 0)

prop_fieldGreaterThanOperator :: Property.NamedProperty
prop_fieldGreaterThanOperator =
  Property.singletonNamedProperty ".> generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" > $1")
      (SO.where_ $ fooField .> 0)

prop_fieldLessThanOrEqualTo :: Property.NamedProperty
prop_fieldLessThanOrEqualTo =
  Property.singletonNamedProperty "fieldLessThanOrEqualTo generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" <= $1")
      (SO.where_ $ SO.fieldLessThanOrEqualTo fooField 0)

prop_fieldLessThanOrEqualToOperator :: Property.NamedProperty
prop_fieldLessThanOrEqualToOperator =
  Property.singletonNamedProperty ".<= generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" <= $1")
      (SO.where_ $ fooField .<= 0)

prop_fieldGreaterThanOrEqualTo :: Property.NamedProperty
prop_fieldGreaterThanOrEqualTo =
  Property.singletonNamedProperty "fieldGreaterThanOrEqualTo generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" >= $1")
      (SO.where_ $ SO.fieldGreaterThanOrEqualTo fooField 0)

prop_fieldGreaterThanOrEqualToOperator :: Property.NamedProperty
prop_fieldGreaterThanOrEqualToOperator =
  Property.singletonNamedProperty ".>= generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" >= $1")
      (SO.where_ $ fooField .>= 0)

prop_fieldLike :: Property.NamedProperty
prop_fieldLike =
  Property.singletonNamedProperty "fieldLike generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" LIKE $1")
      (SO.where_ $ SO.fieldLike fooField $ T.pack "%0%")

prop_fieldLikeInsensitive :: Property.NamedProperty
prop_fieldLikeInsensitive =
  Property.singletonNamedProperty "fieldLike generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" ILIKE $1")
      (SO.where_ $ SO.fieldLikeInsensitive fooField $ T.pack "%0%")

prop_fieldIsNull :: Property.NamedProperty
prop_fieldIsNull =
  Property.singletonNamedProperty "fieldIsNull generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"baz\" IS NULL")
      (SO.where_ $ SO.fieldIsNull bazField)

prop_fieldIsNotNull :: Property.NamedProperty
prop_fieldIsNotNull =
  Property.singletonNamedProperty "fieldIsNotNull generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"baz\" IS NOT NULL")
      (SO.where_ $ SO.fieldIsNotNull bazField)

prop_whereAnd :: Property.NamedProperty
prop_whereAnd =
  Property.singletonNamedProperty "whereAnd generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1) AND (\"bar\" = $2)")
      (SO.where_ $ SO.whereAnd (SO.fieldEquals fooField 10) (SO.fieldEquals barField 20))

prop_whereAndOperator :: Property.NamedProperty
prop_whereAndOperator =
  Property.singletonNamedProperty ".&& generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1) AND (\"bar\" = $2)")
      (SO.where_ $ SO.fieldEquals fooField 10 .&& SO.fieldEquals barField 20)

prop_whereOr :: Property.NamedProperty
prop_whereOr =
  Property.singletonNamedProperty "whereOr generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1) OR (\"bar\" = $2)")
      (SO.where_ $ SO.whereOr (SO.fieldEquals fooField 10) (SO.fieldEquals barField 20))

prop_whereOrOperator :: Property.NamedProperty
prop_whereOrOperator =
  Property.singletonNamedProperty ".|| generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1) OR (\"bar\" = $2)")
      (SO.where_ $ SO.fieldEquals fooField 10 .|| SO.fieldEquals barField 20)

prop_whereAndOrRightAssociativity :: Property.NamedProperty
prop_whereAndOrRightAssociativity =
  Property.singletonNamedProperty ".|| and .&& are right associative" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1) OR ((\"foo\" = $2) AND ((\"foo\" = $3) OR (\"foo\" = $4)))")
      ( SO.where_ $
          SO.fieldEquals fooField 10
            .|| SO.fieldEquals fooField 20
            .&& SO.fieldEquals fooField 30
            .|| SO.fieldEquals fooField 40
      )

prop_equalityAndOrPrecedence :: Property.NamedProperty
prop_equalityAndOrPrecedence =
  Property.singletonNamedProperty ".|| and .&& are have reasonable precedence to combine with .== and friends" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\" = $1) OR ((\"foo\" <> $2) AND ((\"foo\" > $3) OR ((\"foo\" < $4) AND ((\"foo\" >= $5) OR ((\"foo\" <= $6) AND ((\"foo\" IN ($7)) OR (\"foo\" NOT IN ($8))))))))")
      ( SO.where_ $
          fooField .== 10
            .|| fooField ./= 20
            .&& fooField .> 30
            .|| fooField .< 40
            .&& fooField .>= 50
            .|| fooField .<= 60
            .&& fooField .<- (70 :| [])
            .|| fooField .</- (80 :| [])
      )

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
      (Just "WHERE \"foo\" IN ($1)")
      (SO.where_ $ SO.fieldIn fooField (10 :| []))

prop_fieldInOperator :: Property.NamedProperty
prop_fieldInOperator =
  Property.singletonNamedProperty ".<- generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" IN ($1)")
      (SO.where_ $ fooField .<- (10 :| []))

prop_fieldNotIn :: Property.NamedProperty
prop_fieldNotIn =
  Property.singletonNamedProperty "fieldNotIn generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" NOT IN ($1, $2)")
      (SO.where_ $ SO.fieldNotIn fooField (10 :| [20]))

prop_fieldNotInOperator :: Property.NamedProperty
prop_fieldNotInOperator =
  Property.singletonNamedProperty ".</- generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"foo\" NOT IN ($1, $2)")
      (SO.where_ $ fooField .</- (10 :| [20]))

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
      ( SO.orderBy
          ( SO.orderByField fooField Expr.ascendingOrder
              <> SO.orderByField barField Expr.descendingOrder
          )
      )

prop_orderByCombined :: Property.NamedProperty
prop_orderByCombined =
  Property.singletonNamedProperty "orderBy generates expected sql with multiple selectOptions" $
    assertOrderByClauseEquals
      (Just "ORDER BY \"foo\" ASC, \"bar\" DESC")
      ( (SO.orderBy $ SO.orderByColumnName (Expr.columnName "foo") SO.ascendingOrder)
          <> (SO.orderBy $ SO.orderByField barField SO.descendingOrder)
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

assertDistinctEquals :: (HH.MonadTest m, HasCallStack) => String -> SO.SelectOptions -> m ()
assertDistinctEquals mbDistinct selectOptions =
  withFrozenCallStack $
    RawSql.toExampleBytes (SO.selectDistinct selectOptions) HH.=== B8.pack mbDistinct

assertWhereClauseEquals :: (HH.MonadTest m, HasCallStack) => Maybe String -> SO.SelectOptions -> m ()
assertWhereClauseEquals mbWhereClause selectOptions =
  withFrozenCallStack $
    fmap RawSql.toExampleBytes (SO.selectWhereClause selectOptions) HH.=== fmap B8.pack mbWhereClause

assertOrderByClauseEquals :: (HH.MonadTest m, HasCallStack) => Maybe String -> SO.SelectOptions -> m ()
assertOrderByClauseEquals mbOrderByClause selectOptions =
  withFrozenCallStack $
    fmap RawSql.toExampleBytes (SO.selectOrderByClause selectOptions) HH.=== fmap B8.pack mbOrderByClause

assertGroupByClauseEquals :: (HH.MonadTest m, HasCallStack) => Maybe String -> SO.SelectOptions -> m ()
assertGroupByClauseEquals mbGroupByClause selectOptions =
  withFrozenCallStack $
    fmap RawSql.toExampleBytes (SO.selectGroupByClause selectOptions) HH.=== fmap B8.pack mbGroupByClause

fooField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
fooField =
  FieldDef.integerField "foo"

barField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
barField =
  FieldDef.integerField "bar"

bazField :: FieldDef.FieldDefinition FieldDef.Nullable (Maybe Int.Int32)
bazField =
  FieldDef.nullableField $ FieldDef.integerField "baz"
