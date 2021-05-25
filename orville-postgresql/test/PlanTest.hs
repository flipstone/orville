{-# LANGUAGE RankNTypes #-}
module PlanTest
  ( test_plan
  ) where

import           Control.Monad (void)
import           Data.Function (on)
import           Data.Int (Int32)
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Ratio ((%))
import qualified Data.Set as Set
import           Data.Word (Word8)

import qualified Database.Orville.PostgreSQL as O
import qualified Database.Orville.PostgreSQL.Plan as Plan
import qualified Database.Orville.PostgreSQL.Plan.Many as Many

import           Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QC

import qualified TestDB as TestDB

test_plan :: TestTree
test_plan =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "PlanTest"
      [ HUnit.testCase "explain works" $
        let
          actual = Plan.explain treePlan
          expected =
            [ "SELECT \"id\", \"id\", \"tree_id\" FROM \"root\" WHERE (\"id\" = ?)    "
            , "SELECT \"tree_id\", \"id\", \"tree_id\" FROM \"branch\" WHERE (\"tree_id\" = ?)    "
            , "SELECT \"branch_id\", \"id\", \"branch_id\" FROM \"leaf\" WHERE (\"branch_id\" IN (?,?))    "
            ]
         in
           HUnit.assertEqual "Expected explanation to match" expected actual

      , testRootPlan "findMaybeOne" run $
          Plan.findMaybeOne rootTable rootIdField

      , testRootPlan "bind, chain, findMaybeOne" run $
          Plan.bind Plan.askParam $ \param ->
            Plan.chain
              (Plan.use param)
              (Plan.findMaybeOne rootTable rootIdField)

      , testRootPlan "focusParam" run $
          Plan.focusParam rootTreeId (Plan.findMaybeOne treeTable treeIdField)

      , QC.testProperty "many findAll includes all params in map" $ \rootIds ->
        QC.ioProperty $ do
          actual <-
            run $ do
              TestDB.reset testSchema
              Many.toMap <$>
                Plan.execute
                  (Plan.planMany (Plan.findAll rootTable rootIdField))
                  rootIds

          let
            expected :: Map.Map RootId [Root]
            expected =
              Map.fromList $
                map (\rId -> (rId, [])) rootIds

          pure $
            QC.counterexample ("Expected: " ++ show expected) $
            QC.counterexample ("Actual: " ++ show actual) $
            (expected == actual)


      , QC.testProperty "treePlan" $ \expected ->
          let (root, branches, leaves) = mkTreeRecords expected
              leafRatio = (length leaves + 1) % (length branches + 1)
           in QC.ioProperty $ do
                actual <-
                  run $ do
                    TestDB.reset testSchema
                    void (O.insertRecord rootTable root)
                    void (O.insertRecordMany branchTable branches)
                    void (O.insertRecordMany leafTable leaves)
                    Plan.execute treePlan (treeRootId expected)
                pure $
                  QC.classify (length branches > 1) "Num branches > 1" $
                  QC.classify (leafRatio > 1) "Avg leafs/branch > 1" $
                  QC.classify (length branches == 1) "Trivial (Num branches == 1)" $
                  QC.classify (null branches) "Trivial (Num branches == 0)" $
                  QC.counterexample ("Expected: " ++ show expected) $
                  QC.counterexample ("Actual: " ++ show actual) $
                  (expected == actual)
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


testRootPlan :: String
             -> TestDB.OrvilleRunner
             -> (forall scope. Plan.Plan scope RootId (Maybe Root))
             -> TestTree
testRootPlan groupName run plan =
  testGroup groupName
    [ QC.testProperty "One" $ \expected ->
        QC.ioProperty $ do
          actual <- run $ do
            TestDB.reset testSchema
            void (O.insertRecord rootTable expected)
            Plan.execute plan (rootId expected)

          pure $
            QC.counterexample ("Expected: " ++ show expected) $
              QC.counterexample ("Actual: " ++ show actual) $
                (Just expected == actual)

    , QC.testProperty "Many" $ \roots ->
        QC.ioProperty $ do
          let
            uniqRoots =
              List.nubBy ((==) `on` rootId) roots

            expecteds =
              Map.fromList $
                map (\root -> (rootId root, Just root)) uniqRoots

          actuals <- run $ do
            TestDB.reset testSchema
            void (O.insertRecordMany rootTable uniqRoots)
            Many.toMap <$>
              Plan.execute
                (Plan.planMany plan)
                (rootId <$> uniqRoots)


          pure $
            QC.counterexample ("Expected: " ++ show expecteds) $
              QC.counterexample ("Actual: " ++ show actuals) $
                (expecteds == actuals)
    ]

treePlan :: Plan.Plan scope RootId Tree
treePlan =
  Plan.bind (Plan.findOne rootTable rootIdField) $ \root ->
  Plan.bind (Plan.using (rootTreeId <$> root) (Plan.findAll branchTable treeIdField)) $ \branches ->
  Plan.bind (Plan.using (map branchId <$> branches) (Plan.planMany branchPlan)) $ \branchMaps ->
    Tree
      <$> Plan.use (rootTreeId <$> root)
      <*> Plan.use (rootId <$> root)
      <*> Plan.use (Many.toMap <$> branchMaps)

branchPlan :: Plan.Plan scope BranchId (Set.Set LeafId)
branchPlan =
  Set.fromList . map leafId <$>
    Plan.findAll leafTable branchForeignIdField

data Tree = Tree
  { treeId :: TreeId
  , treeRootId :: RootId
  , treeBranches :: Map.Map BranchId (Set.Set LeafId)
  } deriving (Eq, Show)

data Root = Root
  { rootId :: RootId
  , rootTreeId :: TreeId
  } deriving (Eq, Show)

instance QC.Arbitrary Root where
  arbitrary =
    Root <$> QC.arbitrary <*> QC.arbitrary

data Branch = Branch
  { branchId :: BranchId
  , branchTreeId :: TreeId
  } deriving (Eq, Ord, Show)

data Leaf = Leaf
  { leafId :: LeafId
  , leafBranchId :: BranchId
  } deriving (Eq, Show)

newtype RootId = RootId
  { rootIdToInt :: Int32
  } deriving (Eq, Ord, Show, QC.Arbitrary)

newtype TreeId = TreeId
  { treeIdToInt :: Int32
  } deriving (Eq, Ord, Show, QC.Arbitrary)

newtype BranchId = BranchId
  { branchIdToInt :: Int32
  } deriving (Eq, Ord, Show, QC.Arbitrary)

newtype LeafId = LeafId
  { leafIdToInt :: Int32
  } deriving (Eq, Ord, Show, QC.Arbitrary)

idField :: O.FieldDefinition O.NotNull Int32
idField = O.int32Field "id"

rootIdField :: O.FieldDefinition O.NotNull RootId
rootIdField = idField `O.withConversion` O.convertSqlType rootIdToInt RootId

branchIdField :: O.FieldDefinition O.NotNull BranchId
branchIdField =
  idField `O.withConversion` O.convertSqlType branchIdToInt BranchId

branchForeignIdField :: O.FieldDefinition O.NotNull BranchId
branchForeignIdField = branchIdField `O.withName` "branch_id"

leafIdField :: O.FieldDefinition O.NotNull LeafId
leafIdField = idField `O.withConversion` O.convertSqlType leafIdToInt LeafId

treeIdField :: O.FieldDefinition O.NotNull TreeId
treeIdField =
  O.int32Field "tree_id" `O.withConversion` O.convertSqlType treeIdToInt TreeId

testSchema :: O.SchemaDefinition
testSchema = [O.Table rootTable, O.Table branchTable, O.Table leafTable]

rootTable :: O.TableDefinition Root Root RootId
rootTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "root"
    , O.tblPrimaryKey = O.primaryKey rootIdField
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
    , O.tblPrimaryKey = O.primaryKey branchIdField
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
    , O.tblPrimaryKey = O.primaryKey leafIdField
    , O.tblMapper =
        Leaf <$> O.attrField leafId leafIdField <*>
        O.attrField leafBranchId branchForeignIdField
    , O.tblGetKey = leafId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

instance QC.Arbitrary Tree where
  arbitrary =
    mkTree <$> QC.arbitrary <*> QC.arbitrary <*> arbitraryLeafCounts <*>
    (List.nub <$> QC.infiniteListOf QC.arbitrary) <*>
    (List.nub <$> QC.infiniteListOf QC.arbitrary)

type LeafCount = Word8

arbitraryLeafCounts :: QC.Gen [LeafCount]
arbitraryLeafCounts
  -- These scaling factors were turned manually based on the feedback
  -- from the `classify` cases listed in the popper test above to generate
  -- small enough test cases for the tests to run efficiently without only
  -- running trivial cases.
 = QC.scale (`div` 3) (QC.listOf (QC.scale (`div` 4) QC.arbitrary))

mkTree :: TreeId -> RootId -> [LeafCount] -> [BranchId] -> [LeafId] -> Tree
mkTree trId rtId leafCounts brIds lfIds = Tree trId rtId branches
  where
    branches = Map.fromList (zip brIds (branchLeafIds leafCounts lfIds))
    branchLeafIds [] _ = []
    branchLeafIds (count:rest) ids =
      let c = fromInteger . toInteger $ count
       in Set.fromList (take c ids) : branchLeafIds rest (drop c ids)
