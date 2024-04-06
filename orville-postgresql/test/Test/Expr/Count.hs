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

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property

countTests :: Orville.ConnectionPool -> Property.Group
countTests pool =
  Property.group
    "Expr - Count"
    [ prop_count1 pool
    , prop_countColumn pool
    ]

prop_count1 :: Property.NamedDBProperty
prop_count1 =
  Property.singletonNamedDBProperty "SELECT COUNT(1)" $ \pool -> do
    let
      sql =
        Expr.queryExpr
          (Expr.selectClause (Expr.selectExpr Nothing))
          ( Expr.selectDerivedColumns
              [ Expr.deriveColumnAs Expr.count1 (Expr.columnName "count")
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

prop_countColumn :: Property.NamedDBProperty
prop_countColumn =
  Property.singletonNamedDBProperty "In transaction" $ \pool -> do
    let
      sql =
        Expr.queryExpr
          (Expr.selectClause (Expr.selectExpr Nothing))
          ( Expr.selectDerivedColumns
              [ Expr.deriveColumnAs
                  (Expr.countColumn (Orville.fieldColumnName Foo.fooIdField))
                  (Expr.columnName "count")
              ]
          )
          (Just (Expr.tableExpr (Expr.referencesTable $ Orville.tableName Foo.table) Nothing Nothing Nothing Nothing Nothing Nothing))

      marshaller =
        Orville.annotateSqlMarshallerEmptyAnnotation $
          Orville.marshallField id (Orville.integerField "count")

    foos <- HH.forAll (Foo.generateList (Range.linear 0 5))

    result <-
      Foo.withTable pool $ do
        Fold.traverse_ (Orville.insertEntities Foo.table) (NEL.nonEmpty foos)
        Orville.executeAndDecode Orville.SelectQuery sql marshaller

    result === [List.genericLength foos]
