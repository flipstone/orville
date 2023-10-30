module Test.SelectOptions
  ( selectOptionsTests
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH

import Orville.PostgreSQL ((.&&), (./=), (.<), (.<-), (.</-), (.<=), (.==), (.>), (.>=), (.||))
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
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
    , prop_andExpr
    , prop_andExprOperator
    , prop_orExpr
    , prop_orExprOperator
    , prop_andExprOrRightAssociativity
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
      O.emptySelectOptions

prop_fieldEquals :: Property.NamedProperty
prop_fieldEquals =
  Property.singletonNamedProperty "fieldEquals generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") = ($1)")
      (O.where_ $ O.fieldEquals fooField 0)

prop_fieldEqualsOperator :: Property.NamedProperty
prop_fieldEqualsOperator =
  Property.singletonNamedProperty ".== generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") = ($1)")
      (O.where_ $ fooField .== 0)

prop_fieldNotEquals :: Property.NamedProperty
prop_fieldNotEquals =
  Property.singletonNamedProperty "fieldNotEquals generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <> ($1)")
      (O.where_ $ O.fieldNotEquals fooField 0)

prop_fieldNotEqualsOperator :: Property.NamedProperty
prop_fieldNotEqualsOperator =
  Property.singletonNamedProperty "./= generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <> ($1)")
      (O.where_ $ fooField ./= 0)

prop_fieldLessThan :: Property.NamedProperty
prop_fieldLessThan =
  Property.singletonNamedProperty "fieldLessThan generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") < ($1)")
      (O.where_ $ O.fieldLessThan fooField 0)

prop_fieldLessThanOperator :: Property.NamedProperty
prop_fieldLessThanOperator =
  Property.singletonNamedProperty ".< generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") < ($1)")
      (O.where_ $ fooField .< 0)

prop_fieldGreaterThan :: Property.NamedProperty
prop_fieldGreaterThan =
  Property.singletonNamedProperty "fieldGreaterThan generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") > ($1)")
      (O.where_ $ O.fieldGreaterThan fooField 0)

prop_fieldGreaterThanOperator :: Property.NamedProperty
prop_fieldGreaterThanOperator =
  Property.singletonNamedProperty ".> generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") > ($1)")
      (O.where_ $ fooField .> 0)

prop_fieldLessThanOrEqualTo :: Property.NamedProperty
prop_fieldLessThanOrEqualTo =
  Property.singletonNamedProperty "fieldLessThanOrEqualTo generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <= ($1)")
      (O.where_ $ O.fieldLessThanOrEqualTo fooField 0)

prop_fieldLessThanOrEqualToOperator :: Property.NamedProperty
prop_fieldLessThanOrEqualToOperator =
  Property.singletonNamedProperty ".<= generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <= ($1)")
      (O.where_ $ fooField .<= 0)

prop_fieldGreaterThanOrEqualTo :: Property.NamedProperty
prop_fieldGreaterThanOrEqualTo =
  Property.singletonNamedProperty "fieldGreaterThanOrEqualTo generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") >= ($1)")
      (O.where_ $ O.fieldGreaterThanOrEqualTo fooField 0)

prop_fieldGreaterThanOrEqualToOperator :: Property.NamedProperty
prop_fieldGreaterThanOrEqualToOperator =
  Property.singletonNamedProperty ".>= generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") >= ($1)")
      (O.where_ $ fooField .>= 0)

prop_fieldLike :: Property.NamedProperty
prop_fieldLike =
  Property.singletonNamedProperty "fieldLike generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") LIKE ($1)")
      (O.where_ $ O.fieldLike fooField $ T.pack "%0%")

prop_fieldLikeInsensitive :: Property.NamedProperty
prop_fieldLikeInsensitive =
  Property.singletonNamedProperty "fieldLikeInsensitive generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") ILIKE ($1)")
      (O.where_ $ O.fieldLikeInsensitive fooField $ T.pack "%0%")

prop_fieldIsNull :: Property.NamedProperty
prop_fieldIsNull =
  Property.singletonNamedProperty "fieldIsNull generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"baz\" IS NULL")
      (O.where_ $ O.fieldIsNull bazField)

prop_fieldIsNotNull :: Property.NamedProperty
prop_fieldIsNotNull =
  Property.singletonNamedProperty "fieldIsNotNull generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE \"baz\" IS NOT NULL")
      (O.where_ $ O.fieldIsNotNull bazField)

prop_andExpr :: Property.NamedProperty
prop_andExpr =
  Property.singletonNamedProperty "andExpr generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) AND ((\"bar\") = ($2))")
      (O.where_ $ Expr.andExpr (O.fieldEquals fooField 10) (O.fieldEquals barField 20))

prop_andExprOperator :: Property.NamedProperty
prop_andExprOperator =
  Property.singletonNamedProperty ".&& generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) AND ((\"bar\") = ($2))")
      (O.where_ $ O.fieldEquals fooField 10 .&& O.fieldEquals barField 20)

prop_orExpr :: Property.NamedProperty
prop_orExpr =
  Property.singletonNamedProperty "orExpr generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) OR ((\"bar\") = ($2))")
      (O.where_ $ Expr.orExpr (O.fieldEquals fooField 10) (O.fieldEquals barField 20))

prop_orExprOperator :: Property.NamedProperty
prop_orExprOperator =
  Property.singletonNamedProperty ".|| generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) OR ((\"bar\") = ($2))")
      (O.where_ $ O.fieldEquals fooField 10 .|| O.fieldEquals barField 20)

prop_andExprOrRightAssociativity :: Property.NamedProperty
prop_andExprOrRightAssociativity =
  Property.singletonNamedProperty ".|| and .&& are right associative" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) OR (((\"foo\") = ($2)) AND (((\"foo\") = ($3)) OR ((\"foo\") = ($4))))")
      ( O.where_ $
          O.fieldEquals fooField 10
            .|| O.fieldEquals fooField 20
            .&& O.fieldEquals fooField 30
            .|| O.fieldEquals fooField 40
      )

prop_equalityAndOrPrecedence :: Property.NamedProperty
prop_equalityAndOrPrecedence =
  Property.singletonNamedProperty ".|| and .&& are have reasonable precedence to combine with .== and friends" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) OR (((\"foo\") <> ($2)) AND (((\"foo\") > ($3)) OR (((\"foo\") < ($4)) AND (((\"foo\") >= ($5)) OR (((\"foo\") <= ($6)) AND (((\"foo\") IN ($7)) OR ((\"foo\") NOT IN ($8))))))))")
      ( O.where_ $
          fooField
            .== 10
            .|| fooField
            ./= 20
            .&& fooField
            .> 30
            .|| fooField
            .< 40
            .&& fooField
            .>= 50
            .|| fooField
            .<= 60
            .&& fooField
            .<- (70 :| [])
            .|| fooField
            .</- (80 :| [])
      )

prop_whereCombined :: Property.NamedProperty
prop_whereCombined =
  Property.singletonNamedProperty "combining SelectOptions ANDs the where clauses together" $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) AND ((\"bar\") = ($2))")
      ( O.where_ (O.fieldEquals fooField 10)
          <> O.where_ (O.fieldEquals barField 20)
      )

prop_fieldIn :: Property.NamedProperty
prop_fieldIn =
  Property.singletonNamedProperty "fieldIn generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") IN ($1)")
      (O.where_ $ O.fieldIn fooField (10 :| []))

prop_fieldInOperator :: Property.NamedProperty
prop_fieldInOperator =
  Property.singletonNamedProperty ".<- generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") IN ($1)")
      (O.where_ $ fooField .<- (10 :| []))

prop_fieldNotIn :: Property.NamedProperty
prop_fieldNotIn =
  Property.singletonNamedProperty "fieldNotIn generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") NOT IN ($1, $2)")
      (O.where_ $ O.fieldNotIn fooField (10 :| [20]))

prop_fieldNotInOperator :: Property.NamedProperty
prop_fieldNotInOperator =
  Property.singletonNamedProperty ".</- generates expected sql" $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") NOT IN ($1, $2)")
      (O.where_ $ fooField .</- (10 :| [20]))

prop_distinct :: Property.NamedProperty
prop_distinct =
  Property.singletonNamedProperty "distinct generates expected sql" $
    assertDistinctEquals
      ("SELECT DISTINCT ")
      (O.distinct)

prop_orderBy :: Property.NamedProperty
prop_orderBy =
  Property.singletonNamedProperty "orderBy generates expected sql" $
    assertOrderByClauseEquals
      (Just "ORDER BY \"foo\" ASC, \"bar\" DESC")
      ( O.orderBy
          ( O.orderByField fooField Expr.ascendingOrder
              <> O.orderByField barField Expr.descendingOrder
          )
      )

prop_orderByCombined :: Property.NamedProperty
prop_orderByCombined =
  Property.singletonNamedProperty "orderBy generates expected sql with multiple selectOptions" $
    assertOrderByClauseEquals
      (Just "ORDER BY \"foo\" ASC, \"bar\" DESC")
      ( (O.orderBy $ O.orderByColumnName (Expr.columnName "foo") O.ascendingOrder)
          <> (O.orderBy $ O.orderByField barField O.descendingOrder)
      )

prop_groupBy :: Property.NamedProperty
prop_groupBy =
  Property.singletonNamedProperty "groupBy generates expected sql" $
    assertGroupByClauseEquals
      (Just "GROUP BY \"foo\", \"bar\"")
      ( O.groupBy . Expr.groupByColumnsExpr $
          FieldDef.fieldColumnName fooField :| [FieldDef.fieldColumnName barField]
      )

prop_groupByCombined :: Property.NamedProperty
prop_groupByCombined =
  Property.singletonNamedProperty "groupBy generates expected sql with multiple selectOptions" $
    assertGroupByClauseEquals
      (Just "GROUP BY foo, \"bar\"")
      ( (O.groupBy . RawSql.unsafeSqlExpression $ "foo")
          <> (O.groupBy . Expr.groupByColumnsExpr $ (FieldDef.fieldColumnName barField :| []))
      )

assertDistinctEquals :: (HH.MonadTest m, HasCallStack) => String -> O.SelectOptions -> m ()
assertDistinctEquals mbDistinct selectOptions =
  withFrozenCallStack $
    RawSql.toExampleBytes (O.selectDistinct selectOptions) HH.=== B8.pack mbDistinct

assertWhereClauseEquals :: (HH.MonadTest m, HasCallStack) => Maybe String -> O.SelectOptions -> m ()
assertWhereClauseEquals mbWhereClause selectOptions =
  withFrozenCallStack $
    fmap RawSql.toExampleBytes (O.selectWhereClause selectOptions) HH.=== fmap B8.pack mbWhereClause

assertOrderByClauseEquals :: (HH.MonadTest m, HasCallStack) => Maybe String -> O.SelectOptions -> m ()
assertOrderByClauseEquals mbOrderByClause selectOptions =
  withFrozenCallStack $
    fmap RawSql.toExampleBytes (O.selectOrderByClause selectOptions) HH.=== fmap B8.pack mbOrderByClause

assertGroupByClauseEquals :: (HH.MonadTest m, HasCallStack) => Maybe String -> O.SelectOptions -> m ()
assertGroupByClauseEquals mbGroupByClause selectOptions =
  withFrozenCallStack $
    fmap RawSql.toExampleBytes (O.selectGroupByClause selectOptions) HH.=== fmap B8.pack mbGroupByClause

fooField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
fooField =
  FieldDef.integerField "foo"

barField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
barField =
  FieldDef.integerField "bar"

bazField :: FieldDef.FieldDefinition FieldDef.Nullable (Maybe Int.Int32)
bazField =
  FieldDef.nullableField $ FieldDef.integerField "baz"
