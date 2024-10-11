module Test.Expr.Vacuum
  ( vacuumTests
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property

vacuumTests :: Property.Group
vacuumTests =
  Property.group
    "Expr - Vacuum"
    [ prop_vacuumNoOptionsSingleTable
    , prop_vacuumNoOptionsTwoTables
    , prop_vacuumOneOptionSingleTable
    , prop_vacuumOneOptionTwoTables
    , prop_vacuumTwoOptionsOneTable
    , prop_vacuumTwoOptionsTwoTables
    ]

prop_vacuumNoOptionsSingleTable :: Property.NamedProperty
prop_vacuumNoOptionsSingleTable =
  Property.singletonNamedProperty "vacuumExpr with empty options and a single table generates expected sql" $
    assertVacuumEquals
      "VACUUM \"foo\""
      []
      singleTable

prop_vacuumNoOptionsTwoTables :: Property.NamedProperty
prop_vacuumNoOptionsTwoTables =
  Property.singletonNamedProperty "vacuumExpr with empty options and a pair of tables generates expected sql" $
    assertVacuumEquals
      "VACUUM \"foo\", \"bar\""
      []
      twoTables

prop_vacuumOneOptionSingleTable :: Property.NamedProperty
prop_vacuumOneOptionSingleTable =
  Property.singletonNamedProperty "vacuumExpr with one option and a single table generates expected sql" $
    assertVacuumEquals
      "VACUUM (FULL TRUE) \"foo\""
      [Expr.vacuumFull True]
      singleTable

prop_vacuumOneOptionTwoTables :: Property.NamedProperty
prop_vacuumOneOptionTwoTables =
  Property.singletonNamedProperty "vacuumExpr with one option and a pair of tables generates expected sql" $
    assertVacuumEquals
      "VACUUM (FULL TRUE) \"foo\", \"bar\""
      [Expr.vacuumFull True]
      twoTables

prop_vacuumTwoOptionsOneTable :: Property.NamedProperty
prop_vacuumTwoOptionsOneTable =
  Property.singletonNamedProperty "vacuumExpr with two options and a single table generates expected sql" $
    assertVacuumEquals
      "VACUUM (FULL TRUE, VERBOSE TRUE) \"foo\""
      [ Expr.vacuumFull True
      , Expr.vacuumVerbose True
      ]
      singleTable

prop_vacuumTwoOptionsTwoTables :: Property.NamedProperty
prop_vacuumTwoOptionsTwoTables =
  Property.singletonNamedProperty "vacuumExpr with two options and a pair of tables generates expected sql" $
    assertVacuumEquals
      "VACUUM (FULL TRUE, VERBOSE TRUE) \"foo\", \"bar\""
      [ Expr.vacuumFull True
      , Expr.vacuumVerbose True
      ]
      twoTables

assertVacuumEquals :: (HH.MonadTest m, HasCallStack) => String -> [Expr.VacuumOption] -> NonEmpty (Expr.QualifiedOrUnqualified Expr.TableName) -> m ()
assertVacuumEquals mbVacuum vacuumOptions vacuumTables =
  withFrozenCallStack $
    RawSql.toExampleBytes (Expr.vacuumExpr vacuumOptions vacuumTables) HH.=== B8.pack mbVacuum

singleTable :: NonEmpty (Expr.QualifiedOrUnqualified Expr.TableName)
singleTable = pure . Expr.unqualified $ Expr.tableName "foo"

twoTables :: NonEmpty (Expr.QualifiedOrUnqualified Expr.TableName)
twoTables =
  (Expr.unqualified $ Expr.tableName "foo") :| [Expr.unqualified $ Expr.tableName "bar"]
