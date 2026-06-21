{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.TypeId
  ( typeIdTests
  )
where

import qualified Data.KindID as KindID
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH
import qualified Orville.TypeId.FieldDefinition as TypeIdFieldDefinition
import qualified Orville.TypeId.FunctionDefinitions as TypeIdFunctionDefinitions

typeIdTests :: Orville.ConnectionPool -> Tasty.TestTree
typeIdTests pool =
  Tasty.testGroup
    "TypeID"
    [ TastyHH.testProperty
        "Checks that a reasonable value is generated from a KindID default value"
        (prop_checkKindIDDefaultValue pool)
    ]

prop_checkKindIDDefaultValue :: Orville.ConnectionPool -> HH.Property
prop_checkKindIDDefaultValue pool =
  HH.withTests 1 . HH.property $ do
    -- Load required functions
    HH.evalIO $ Orville.runOrville pool $ do
      Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateFunctionExpr TypeIdFunctionDefinitions.uuidGenerateV7 (Just Expr.orReplace)
      Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateFunctionExpr TypeIdFunctionDefinitions.base32Encode (Just Expr.orReplace)
      Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateFunctionExpr TypeIdFunctionDefinitions.base32Decode (Just Expr.orReplace)
      Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateFunctionExpr TypeIdFunctionDefinitions.typeIdPrint (Just Expr.orReplace)
      Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateFunctionExpr TypeIdFunctionDefinitions.typeIdGenerateText (Just Expr.orReplace)

    let
      tableNameStr = "test_kindid_default_table"
      tableName = Expr.unqualified $ Expr.tableName tableNameStr
      idField = TypeIdFieldDefinition.kindIDFieldDefinitionWithDefault @"test_prefix" "id"
      table :: Orville.TableDefinition (Orville.HasKey (KindID.KindID "test_prefix")) writeEntity (KindID.KindID "test_prefix")
      table =
        Orville.mkTableDefinition
          tableNameStr
          (Orville.primaryKey idField)
          (Orville.marshallReadOnlyField idField)

    insertedEntity <- HH.evalIO . Orville.runOrville pool $ do
      Orville.executeVoid Orville.DDLQuery $ Expr.dropTableExpr (Just Expr.ifExists) tableName
      Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateTableExpr table
      Orville.insertAndReturnEntity table ()

    HH.annotate . T.unpack $ KindID.toText insertedEntity
    T.length (KindID.toText insertedEntity) === (11 + 1 + 26) -- prefix + underscore + uuid encoded suffix
    KindID.getPrefix insertedEntity === T.pack "test_prefix"
