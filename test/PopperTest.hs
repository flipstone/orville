module PopperTest where

import Data.Int (Int32)
import Database.Orville as O
import Database.Orville.Popper as P

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

test_popper :: TestTree
test_popper =
  testGroup
    "PopperTest"
    [ testCase "explainLines works" $
      let popper =
            P.hasOne' rootTable idField >>>
            P.fromKern rootTreeId >>>
            P.hasMany branchTable treeIdField >>>
            P.fromKern (map branchId) >>>
            P.popMany (P.hasMany leafTable branchIdField)
          actual = P.explainLines popper
          expected =
            [ "fetch one root by one id "
            , "fetch many branch by one tree_id "
            , "fetch many leaf by many branch_id (*-1) "
            ]
       in assertEqual "Expected explanation to match" expected actual
    ]

data Root = Root
  { rootId :: Int32
  , rootTreeId :: Int32
  }

data Branch = Branch
  { branchId :: Int32
  , branchTreeId :: Int32
  }

data Leaf = Leaf
  { leafId :: Int32
  , leafBranchId :: Int32
  }

idField :: O.FieldDefinition Int32
idField = O.int32Field "id"

treeIdField :: O.FieldDefinition Int32
treeIdField = idField `O.withName` "tree_id"

branchIdField :: O.FieldDefinition Int32
branchIdField = idField `O.withName` "branch_id"

rootTable :: O.TableDefinition Root Root Int32
rootTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "root"
    , O.tblPrimaryKey = idField
    , O.tblMapper =
        Root <$> O.attrField rootId idField <*>
        O.attrField rootTreeId treeIdField
    , O.tblGetKey = rootId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

branchTable :: O.TableDefinition Branch Branch Int32
branchTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "branch"
    , O.tblPrimaryKey = idField
    , O.tblMapper =
        Branch <$> O.attrField branchId idField <*>
        O.attrField branchTreeId treeIdField
    , O.tblGetKey = branchId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

leafTable :: O.TableDefinition Leaf Leaf Int32
leafTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "leaf"
    , O.tblPrimaryKey = idField
    , O.tblMapper =
        Leaf <$> O.attrField leafId idField <*>
        O.attrField leafBranchId branchIdField
    , O.tblGetKey = leafId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }
