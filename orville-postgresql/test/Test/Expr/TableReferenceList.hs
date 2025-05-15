module Test.Expr.TableReferenceList
  ( tableReferenceListTests
  ) where

import qualified Data.ByteString.Char8 as B8
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property

tableReferenceListTests :: Property.Group
tableReferenceListTests =
  Property.group
    "Expr - TableReferenceList"
    [ prop_tableReferenceList
    ]

prop_tableReferenceList :: Property.NamedProperty
prop_tableReferenceList =
  Property.singletonNamedProperty "creates a comma delimited table list"
    $ assertTableReferenteListEquals
      "\"foo\", \"bar\""
    $ Expr.tableReferenceList [fooTable, barTable]

assertTableReferenteListEquals ::
  (HH.MonadTest m, HasCallStack) =>
  String ->
  Expr.TableReferenceList ->
  m ()
assertTableReferenteListEquals expectedString tableReferenceList =
  withFrozenCallStack $
    RawSql.toExampleBytes tableReferenceList === B8.pack expectedString

fooTable :: Expr.TableReference
fooTable = Expr.tableNameReference (Expr.unqualified $ Expr.tableName "foo") Nothing

barTable :: Expr.TableReference
barTable = Expr.tableNameReference (Expr.unqualified $ Expr.tableName "bar") Nothing
