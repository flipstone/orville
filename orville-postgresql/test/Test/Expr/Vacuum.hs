module Test.Expr.Vacuum
  ( vacuumTests
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property

vacuumTests :: Tasty.TestTree
vacuumTests =
  Tasty.testGroup
    "Expr - Vacuum"
    [ TastyHH.testProperty "vacuumExpr with empty options and a single table generates expected sql" prop_vacuumNoOptionsSingleTable
    , TastyHH.testProperty "vacuumExpr with empty options and a pair of tables generates expected sql" prop_vacuumNoOptionsTwoTables
    , TastyHH.testProperty "vacuumExpr with one option and a single table generates expected sql" prop_vacuumOneOptionSingleTable
    , TastyHH.testProperty "vacuumExpr with one option and a pair of tables generates expected sql" prop_vacuumOneOptionTwoTables
    , TastyHH.testProperty "vacuumExpr with two options and a single table generates expected sql" prop_vacuumTwoOptionsOneTable
    , TastyHH.testProperty "vacuumExpr with two options and a pair of tables generates expected sql" prop_vacuumTwoOptionsTwoTables
    ]

prop_vacuumNoOptionsSingleTable :: HH.Property
prop_vacuumNoOptionsSingleTable =
  Property.singletonProperty $
    assertVacuumEquals
      "VACUUM \"foo\""
      []
      singleTable

prop_vacuumNoOptionsTwoTables :: HH.Property
prop_vacuumNoOptionsTwoTables =
  Property.singletonProperty $
    assertVacuumEquals
      "VACUUM \"foo\", \"bar\""
      []
      twoTables

prop_vacuumOneOptionSingleTable :: HH.Property
prop_vacuumOneOptionSingleTable =
  Property.singletonProperty $
    assertVacuumEquals
      "VACUUM (FULL TRUE) \"foo\""
      [Expr.vacuumFull True]
      singleTable

prop_vacuumOneOptionTwoTables :: HH.Property
prop_vacuumOneOptionTwoTables =
  Property.singletonProperty $
    assertVacuumEquals
      "VACUUM (FULL TRUE) \"foo\", \"bar\""
      [Expr.vacuumFull True]
      twoTables

prop_vacuumTwoOptionsOneTable :: HH.Property
prop_vacuumTwoOptionsOneTable =
  Property.singletonProperty $
    assertVacuumEquals
      "VACUUM (FULL TRUE, VERBOSE TRUE) \"foo\""
      [ Expr.vacuumFull True
      , Expr.vacuumVerbose True
      ]
      singleTable

prop_vacuumTwoOptionsTwoTables :: HH.Property
prop_vacuumTwoOptionsTwoTables =
  Property.singletonProperty $
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
