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
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import Orville.PostgreSQL ((.&&), (./=), (.<), (.<-), (.</-), (.<=), (.==), (.>), (.>=), (.||))
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Marshall.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.Property as Property

selectOptionsTests :: Tasty.TestTree
selectOptionsTests =
  Tasty.testGroup
    "SelectOptions"
    [ TastyHH.testProperty
        "emptySelectOptions yields no whereClause"
        prop_emptyWhereClause
    , TastyHH.testProperty
        "fieldEquals generates expected sql"
        prop_fieldEquals
    , TastyHH.testProperty
        ".== generates expected sql"
        prop_fieldEqualsOperator
    , TastyHH.testProperty
        "fieldNotEquals generates expected sql"
        prop_fieldNotEquals
    , TastyHH.testProperty
        "fieldIsDistinctFrom generates expected sql"
        prop_fieldIsDistinctFrom
    , TastyHH.testProperty
        "fieldIsNotDistinctFrom generates expected sql"
        prop_fieldIsNotDistinctFrom
    , TastyHH.testProperty
        "./= generates expected sql"
        prop_fieldNotEqualsOperator
    , TastyHH.testProperty
        "fieldLessThan generates expected sql"
        prop_fieldLessThan
    , TastyHH.testProperty
        ".< generates expected sql"
        prop_fieldLessThanOperator
    , TastyHH.testProperty
        "fieldGreaterThan generates expected sql"
        prop_fieldGreaterThan
    , TastyHH.testProperty
        ".> generates expected sql"
        prop_fieldGreaterThanOperator
    , TastyHH.testProperty
        "fieldLessThanOrEqualTo generates expected sql"
        prop_fieldLessThanOrEqualTo
    , TastyHH.testProperty
        ".<= generates expected sql"
        prop_fieldLessThanOrEqualToOperator
    , TastyHH.testProperty
        "fieldGreaterThanOrEqualTo generates expected sql"
        prop_fieldGreaterThanOrEqualTo
    , TastyHH.testProperty
        ".>= generates expected sql"
        prop_fieldGreaterThanOrEqualToOperator
    , TastyHH.testProperty
        "fieldIsNull generates expected sql"
        prop_fieldIsNull
    , TastyHH.testProperty
        "fieldIsNotNull generates expected sql"
        prop_fieldIsNotNull
    , TastyHH.testProperty
        "fieldLike generates expected sql"
        prop_fieldLike
    , TastyHH.testProperty
        "fieldLikeInsensitive generates expected sql"
        prop_fieldLikeInsensitive
    , TastyHH.testProperty
        "andExpr generates expected sql"
        prop_andExpr
    , TastyHH.testProperty
        ".&& generates expected sql"
        prop_andExprOperator
    , TastyHH.testProperty
        "orExpr generates expected sql"
        prop_orExpr
    , TastyHH.testProperty
        ".|| generates expected sql"
        prop_orExprOperator
    , TastyHH.testProperty
        ".|| and .&& are right associative"
        prop_andExprOrRightAssociativity
    , TastyHH.testProperty
        ".|| and .&& are have reasonable precedence to combine with .== and friends"
        prop_equalityAndOrPrecedence
    , TastyHH.testProperty
        "combining SelectOptions ANDs the where clauses together"
        prop_whereCombined
    , TastyHH.testProperty
        "fieldIn generates expected sql"
        prop_fieldIn
    , TastyHH.testProperty
        ".<- generates expected sql"
        prop_fieldInOperator
    , TastyHH.testProperty
        "fieldNotIn generates expected sql"
        prop_fieldNotIn
    , TastyHH.testProperty
        ".</- generates expected sql"
        prop_fieldNotInOperator
    , TastyHH.testProperty
        "distinct generates expected sql"
        prop_distinct
    , TastyHH.testProperty
        "orderBy generates expected sql"
        prop_orderBy
    , TastyHH.testProperty
        "orderBy generates expected sql"
        prop_orderByQualified
    , TastyHH.testProperty
        "orderBy generates expected sql with multiple selectOptions"
        prop_orderByCombined
    , TastyHH.testProperty
        "groupBy generates expected sql"
        prop_groupBy
    , TastyHH.testProperty
        "groupBy generates expected sql with multiple selectOptions"
        prop_groupByCombined
    , TastyHH.testProperty
        "forRowLock generates expected sql"
        prop_forRowLock
    ]

prop_emptyWhereClause :: HH.Property
prop_emptyWhereClause =
  Property.singletonProperty $
    assertWhereClauseEquals
      Nothing
      O.emptySelectOptions

prop_fieldEquals :: HH.Property
prop_fieldEquals =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") = ($1)")
      (O.where_ $ O.fieldEquals fooField 0)

prop_fieldEqualsOperator :: HH.Property
prop_fieldEqualsOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") = ($1)")
      (O.where_ $ fooField .== 0)

prop_fieldNotEquals :: HH.Property
prop_fieldNotEquals =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <> ($1)")
      (O.where_ $ O.fieldNotEquals fooField 0)

prop_fieldIsDistinctFrom :: HH.Property
prop_fieldIsDistinctFrom =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") IS DISTINCT FROM ($1)")
      (O.where_ $ O.fieldIsDistinctFrom fooField 0)

prop_fieldIsNotDistinctFrom :: HH.Property
prop_fieldIsNotDistinctFrom =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") IS NOT DISTINCT FROM ($1)")
      (O.where_ $ O.fieldIsNotDistinctFrom fooField 0)

prop_fieldNotEqualsOperator :: HH.Property
prop_fieldNotEqualsOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <> ($1)")
      (O.where_ $ fooField ./= 0)

prop_fieldLessThan :: HH.Property
prop_fieldLessThan =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") < ($1)")
      (O.where_ $ O.fieldLessThan fooField 0)

prop_fieldLessThanOperator :: HH.Property
prop_fieldLessThanOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") < ($1)")
      (O.where_ $ fooField .< 0)

prop_fieldGreaterThan :: HH.Property
prop_fieldGreaterThan =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") > ($1)")
      (O.where_ $ O.fieldGreaterThan fooField 0)

prop_fieldGreaterThanOperator :: HH.Property
prop_fieldGreaterThanOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") > ($1)")
      (O.where_ $ fooField .> 0)

prop_fieldLessThanOrEqualTo :: HH.Property
prop_fieldLessThanOrEqualTo =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <= ($1)")
      (O.where_ $ O.fieldLessThanOrEqualTo fooField 0)

prop_fieldLessThanOrEqualToOperator :: HH.Property
prop_fieldLessThanOrEqualToOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") <= ($1)")
      (O.where_ $ fooField .<= 0)

prop_fieldGreaterThanOrEqualTo :: HH.Property
prop_fieldGreaterThanOrEqualTo =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") >= ($1)")
      (O.where_ $ O.fieldGreaterThanOrEqualTo fooField 0)

prop_fieldGreaterThanOrEqualToOperator :: HH.Property
prop_fieldGreaterThanOrEqualToOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") >= ($1)")
      (O.where_ $ fooField .>= 0)

prop_fieldLike :: HH.Property
prop_fieldLike =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") LIKE ($1)")
      (O.where_ $ O.fieldLike fooField $ T.pack "%0%")

prop_fieldLikeInsensitive :: HH.Property
prop_fieldLikeInsensitive =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") ILIKE ($1)")
      (O.where_ $ O.fieldLikeInsensitive fooField $ T.pack "%0%")

prop_fieldIsNull :: HH.Property
prop_fieldIsNull =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE \"baz\" IS NULL")
      (O.where_ $ O.fieldIsNull bazField)

prop_fieldIsNotNull :: HH.Property
prop_fieldIsNotNull =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE \"baz\" IS NOT NULL")
      (O.where_ $ O.fieldIsNotNull bazField)

prop_andExpr :: HH.Property
prop_andExpr =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) AND ((\"bar\") = ($2))")
      (O.where_ $ Expr.andExpr (O.fieldEquals fooField 10) (O.fieldEquals barField 20))

prop_andExprOperator :: HH.Property
prop_andExprOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) AND ((\"bar\") = ($2))")
      (O.where_ $ O.fieldEquals fooField 10 .&& O.fieldEquals barField 20)

prop_orExpr :: HH.Property
prop_orExpr =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) OR ((\"bar\") = ($2))")
      (O.where_ $ Expr.orExpr (O.fieldEquals fooField 10) (O.fieldEquals barField 20))

prop_orExprOperator :: HH.Property
prop_orExprOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) OR ((\"bar\") = ($2))")
      (O.where_ $ O.fieldEquals fooField 10 .|| O.fieldEquals barField 20)

prop_andExprOrRightAssociativity :: HH.Property
prop_andExprOrRightAssociativity =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) OR (((\"foo\") = ($2)) AND (((\"foo\") = ($3)) OR ((\"foo\") = ($4))))")
      ( O.where_ $
          O.fieldEquals fooField 10
            .|| O.fieldEquals fooField 20
            .&& O.fieldEquals fooField 30
            .|| O.fieldEquals fooField 40
      )

prop_equalityAndOrPrecedence :: HH.Property
prop_equalityAndOrPrecedence =
  Property.singletonProperty $
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

prop_whereCombined :: HH.Property
prop_whereCombined =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE ((\"foo\") = ($1)) AND ((\"bar\") = ($2))")
      ( O.where_ (O.fieldEquals fooField 10)
          <> O.where_ (O.fieldEquals barField 20)
      )

prop_fieldIn :: HH.Property
prop_fieldIn =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") IN ($1)")
      (O.where_ $ O.fieldIn fooField (10 :| []))

prop_fieldInOperator :: HH.Property
prop_fieldInOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") IN ($1)")
      (O.where_ $ fooField .<- (10 :| []))

prop_fieldNotIn :: HH.Property
prop_fieldNotIn =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") NOT IN ($1, $2)")
      (O.where_ $ O.fieldNotIn fooField (10 :| [20]))

prop_fieldNotInOperator :: HH.Property
prop_fieldNotInOperator =
  Property.singletonProperty $
    assertWhereClauseEquals
      (Just "WHERE (\"foo\") NOT IN ($1, $2)")
      (O.where_ $ fooField .</- (10 :| [20]))

prop_distinct :: HH.Property
prop_distinct =
  Property.singletonProperty $
    assertDistinctEquals
      ("SELECT DISTINCT ")
      (O.distinct)

prop_orderBy :: HH.Property
prop_orderBy =
  Property.singletonProperty $
    assertOrderByClauseEquals
      (Just "ORDER BY \"foo\" ASC, \"bar\" DESC")
      ( O.orderBy
          ( O.orderByField fooField Expr.ascendingOrder
              <> O.orderByField barField Expr.descendingOrder
          )
      )

prop_orderByQualified :: HH.Property
prop_orderByQualified =
  Property.singletonProperty $
    assertOrderByClauseEquals
      (Just "ORDER BY \"f\".\"foo\" ASC, \"bar\" DESC")
      ( O.orderBy
          ( O.orderBySqlComparable (O.qualifyField (Marshall.stringToAliasName "f") fooField) Expr.ascendingOrder
              <> O.orderByField barField Expr.descendingOrder
          )
      )

prop_orderByCombined :: HH.Property
prop_orderByCombined =
  Property.singletonProperty $
    assertOrderByClauseEquals
      (Just "ORDER BY \"foo\" ASC, \"bar\" DESC")
      ( (O.orderBy $ O.orderByColumnName (Expr.unqualified (Expr.columnName "foo")) O.ascendingOrder)
          <> (O.orderBy $ O.orderByField barField O.descendingOrder)
      )

prop_groupBy :: HH.Property
prop_groupBy =
  Property.singletonProperty $
    assertGroupByClauseEquals
      (Just "GROUP BY \"foo\", \"bar\"")
      ( O.groupBy . Expr.groupByColumnsExpr $
          Expr.unqualified (FieldDef.fieldColumnName fooField)
            :| [Expr.unqualified (FieldDef.fieldColumnName barField)]
      )

prop_groupByCombined :: HH.Property
prop_groupByCombined =
  Property.singletonProperty $
    assertGroupByClauseEquals
      (Just "GROUP BY foo, \"bar\"")
      ( (O.groupBy . RawSql.unsafeSqlExpression $ "foo")
          <> (O.groupBy . Expr.groupByColumnsExpr $ ((Expr.unqualified (FieldDef.fieldColumnName barField)) :| []))
      )

prop_forRowLock :: HH.Property
prop_forRowLock =
  Property.singletonProperty $
    assertRowLockingClauseEquals
      (Just "FOR UPDATE SKIP LOCKED")
      (O.forRowLock $ Expr.rowLockingClause Expr.updateStrength Nothing (Just Expr.skipLockedRowLockingOption))

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

assertRowLockingClauseEquals :: (HH.MonadTest m, HasCallStack) => Maybe String -> O.SelectOptions -> m ()
assertRowLockingClauseEquals mbRowLockingClause selectOptions =
  withFrozenCallStack $
    fmap RawSql.toExampleBytes (O.selectRowLockingClause selectOptions) HH.=== fmap B8.pack mbRowLockingClause

fooField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
fooField =
  FieldDef.integerField "foo"

barField :: FieldDef.FieldDefinition FieldDef.NotNull Int.Int32
barField =
  FieldDef.integerField "bar"

bazField :: FieldDef.FieldDefinition FieldDef.Nullable (Maybe Int.Int32)
bazField =
  FieldDef.nullableField $ FieldDef.integerField "baz"
