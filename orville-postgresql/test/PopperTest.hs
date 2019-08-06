module PopperTest where

import Control.Monad (void)
import Data.Int (Int32)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Word (Word8)
import qualified Database.Orville.PostgreSQL as O
import Database.Orville.PostgreSQL.Popper ((>>>))
import qualified Database.Orville.PostgreSQL.Popper as P

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (Arbitrary(arbitrary), testProperty)
import Test.Tasty.QuickCheck as QC

import qualified TestDB as TestDB

test_popper :: TestTree
test_popper =
  testGroup
    "PopperTest"
    [ testCase "explainLines works" $
      let actual = P.explainLines treeRootPopper
          expected =
            [ "fetch one root by one id "
            , "fetch many branch by one tree_id "
            , "fetch many leaf by many branch_id (*-1) "
            ]
       in assertEqual "Expected explanation to match" expected actual
    , TestDB.withOrvilleRun $ \run ->
        testProperty "treeRootPopper" $ \expected ->
          let (root, branches, leaves) = mkTreeRecords expected
              leafRatio = (length leaves + 1) % (length branches + 1)
           in QC.ioProperty $ do
                actual <-
                  run $ do
                    TestDB.reset testSchema
                    void (O.insertRecord rootTable root)
                    void (O.insertRecordMany branchTable branches)
                    void (O.insertRecordMany leafTable leaves)
                    P.popThrow treeRootPopper (treeRootId expected)
                pure $
                  QC.classify (length branches > 1) "Num branches > 1" $
                  QC.classify (leafRatio > 1) "Avg leafs/branch > 1" $
                  QC.classify
                    (length branches == 1)
                    "Trivial (Num branches == 1)" $
                  QC.classify (null branches) "Trivial (Num branches == 0)" $
                  QC.counterexample
                    ("Actual: " ++ show actual)
                    (expected == actual)
    , TestDB.withOrvilleRun $ \run ->
        testProperty "popMany pure doesn't cause infinite loop" $ \count -> do
          let roots1Pooper =
                P.popMany (pure (RootId 1) >>> P.hasOne rootTable rootIdField)
              inputs = replicate count ()
              expected = replicate count Nothing
          QC.ioProperty $ do
            actual <-
              run $ do
                TestDB.reset testSchema
                P.popThrow roots1Pooper inputs
            pure (expected == actual)
    ]

-- mkTreeRecords projects a Tree into the Root, Branch and Leaf entities
-- that should be recorded in the database to reflect the structure of
-- the Tree. Using the treeRootPopper below should reassemble these
-- entities into the original tree.
--
mkTreeRecords :: Tree -> (Root, [Branch], [Leaf])
mkTreeRecords tree = (root, branches, concat leafLists)
  where
    root = Root (treeRootId tree) (treeId tree)
    (branches, leafLists) =
      Map.foldrWithKey foldBranches ([], []) (treeBranches tree)
    foldBranches bId lfIds (bAccum, lAccum) =
      let branch = Branch bId (treeId tree)
          leaves = map (flip Leaf bId) (Set.toList lfIds)
       in (branch : bAccum, leaves : lAccum)

treeRootPopper :: P.Popper RootId Tree
treeRootPopper = P.hasOne' rootTable rootIdField >>> rootPopper

rootPopper :: P.Popper Root Tree
rootPopper =
  Tree <$> P.fromKern rootTreeId <*> P.fromKern rootId <*>
  (P.fromKern rootTreeId >>>
   P.hasMany branchTable treeIdField >>>
   P.popMany branchPopper >>> P.fromKern Map.fromList)

branchPopper :: P.Popper Branch (BranchId, Set.Set LeafId)
branchPopper =
  pair <$> P.fromKern branchId <*>
  (Set.fromList <$>
   (P.fromKern branchId >>>
    P.hasMany leafTable branchForeignIdField >>> P.fromKern (map leafId)))
  where
    pair x y = (x, y)

data Tree = Tree
  { treeId :: TreeId
  , treeRootId :: RootId
  , treeBranches :: Map.Map BranchId (Set.Set LeafId)
  } deriving (Eq, Show)

data Root = Root
  { rootId :: RootId
  , rootTreeId :: TreeId
  } deriving (Eq, Show)

data Branch = Branch
  { branchId :: BranchId
  , branchTreeId :: TreeId
  } deriving (Eq, Show)

data Leaf = Leaf
  { leafId :: LeafId
  , leafBranchId :: BranchId
  } deriving (Eq, Show)

newtype RootId = RootId
  { rootIdToInt :: Int32
  } deriving (Eq, Ord, Show, Arbitrary)

newtype TreeId = TreeId
  { treeIdToInt :: Int32
  } deriving (Eq, Ord, Show, Arbitrary)

newtype BranchId = BranchId
  { branchIdToInt :: Int32
  } deriving (Eq, Ord, Show, Arbitrary)

newtype LeafId = LeafId
  { leafIdToInt :: Int32
  } deriving (Eq, Ord, Show, Arbitrary)

idField :: O.FieldDefinition Int32
idField = O.int32Field "id"

rootIdField :: O.FieldDefinition RootId
rootIdField = idField `O.withConversion` O.convertSqlType rootIdToInt RootId

branchIdField :: O.FieldDefinition BranchId
branchIdField =
  idField `O.withConversion` O.convertSqlType branchIdToInt BranchId

branchForeignIdField :: O.FieldDefinition BranchId
branchForeignIdField = branchIdField `O.withName` "branch_id"

leafIdField :: O.FieldDefinition LeafId
leafIdField = idField `O.withConversion` O.convertSqlType leafIdToInt LeafId

treeIdField :: O.FieldDefinition TreeId
treeIdField =
  O.int32Field "tree_id" `O.withConversion` O.convertSqlType treeIdToInt TreeId

testSchema :: O.SchemaDefinition
testSchema = [O.Table rootTable, O.Table branchTable, O.Table leafTable]

rootTable :: O.TableDefinition Root Root RootId
rootTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "root"
    , O.tblPrimaryKey = rootIdField
    , O.tblMapper =
        Root <$> O.attrField rootId rootIdField <*>
        O.attrField rootTreeId treeIdField
    , O.tblGetKey = rootId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

branchTable :: O.TableDefinition Branch Branch BranchId
branchTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "branch"
    , O.tblPrimaryKey = branchIdField
    , O.tblMapper =
        Branch <$> O.attrField branchId branchIdField <*>
        O.attrField branchTreeId treeIdField
    , O.tblGetKey = branchId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

leafTable :: O.TableDefinition Leaf Leaf LeafId
leafTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "leaf"
    , O.tblPrimaryKey = leafIdField
    , O.tblMapper =
        Leaf <$> O.attrField leafId leafIdField <*>
        O.attrField leafBranchId branchForeignIdField
    , O.tblGetKey = leafId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

instance Arbitrary Tree where
  arbitrary =
    mkTree <$> arbitrary <*> arbitrary <*> arbitraryLeafCounts <*>
    (List.nub <$> infiniteListOf arbitrary) <*>
    (List.nub <$> infiniteListOf arbitrary)

type LeafCount = Word8

arbitraryLeafCounts :: QC.Gen [LeafCount]
arbitraryLeafCounts
  -- These scaling factors were turned manually based on the feedback
  -- from the `classify` cases listed in the popper test above to generate
  -- small enough test cases for the tests to run efficiently without only
  -- running trivial cases.
 = QC.scale (`div` 3) (QC.listOf (QC.scale (`div` 4) arbitrary))

mkTree :: TreeId -> RootId -> [LeafCount] -> [BranchId] -> [LeafId] -> Tree
mkTree trId rtId leafCounts brIds lfIds = Tree trId rtId branches
  where
    branches = Map.fromList (zip brIds (branchLeafIds leafCounts lfIds))
    branchLeafIds [] _ = []
    branchLeafIds (count:rest) ids =
      let c = fromInteger . toInteger $ count
       in Set.fromList (take c ids) : branchLeafIds rest (drop c ids)
