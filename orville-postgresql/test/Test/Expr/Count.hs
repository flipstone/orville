module Test.Expr.Count
  ( countTests
  )
where

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property

countTests :: Orville.ConnectionPool -> Tasty.TestTree
countTests pool =
  Tasty.testGroup
    "Expr - Count"
    [ TastyHH.testProperty "SELECT COUNT(1)" (prop_count1 pool)
    , TastyHH.testProperty "SELECT COUNT(column)" (prop_countColumn pool)
    ]

prop_count1 :: Orville.ConnectionPool -> HH.Property
prop_count1 pool =
  Property.singletonProperty $ do
    let
      sql =
        Expr.queryExpr
          (Expr.selectClause (Expr.selectExpr Nothing))
          ( Expr.selectDerivedColumns
              [ Expr.deriveColumnAs Expr.count1AggregateFunction (Expr.columnName "count")
              ]
          )
          Nothing

      marshaller =
        Orville.annotateSqlMarshallerEmptyAnnotation $
          Orville.marshallField id (Orville.integerField "count")

    result <-
      HH.evalIO $
        Orville.runOrville pool $
          Orville.executeAndDecode Orville.SelectQuery sql marshaller

    result === [1]

prop_countColumn :: Orville.ConnectionPool -> HH.Property
prop_countColumn pool =
  Property.singletonProperty $ do
    let
      sql =
        Expr.queryExpr
          (Expr.selectClause (Expr.selectExpr Nothing))
          ( Expr.selectDerivedColumns
              [ Expr.deriveColumnAs
                  (Expr.countColumnAggregateFunction . Expr.unqualified $ Orville.fieldColumnName Foo.fooIdField)
                  (Expr.columnName "count")
              ]
          )
          ( Just
              ( Expr.tableExpr
                  (Expr.singleTableReferenceList (Orville.tableName Foo.table))
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
                  Nothing
              )
          )

      marshaller =
        Orville.annotateSqlMarshallerEmptyAnnotation $
          Orville.marshallField id (Orville.integerField "count")

    foos <- HH.forAll (Foo.generateList (Range.linear 0 5))

    result <-
      Foo.withTable pool $ do
        Fold.traverse_ (Orville.insertEntities Orville.InOneStatement Foo.table) (NEL.nonEmpty foos)
        Orville.executeAndDecode Orville.SelectQuery sql marshaller

    result === [List.genericLength foos]
