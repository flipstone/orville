{-# LANGUAGE CPP #-}
module ConduitTest where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (fuse, runConduit)
import Data.Conduit.List (consume)

import qualified Database.Orville.PostgreSQL as O
import qualified Database.Orville.PostgreSQL.Conduit as OC
import Database.Orville.PostgreSQL.ResourceT ()
import qualified Database.Orville.PostgreSQL.Select as OS

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified TestDB

import AppManagedEntity.Data.Virus (bpsVirus, brnVirus, virusId)
import AppManagedEntity.Schema (schema, virusIdField, virusTable)

test_conduit :: TestTree
test_conduit =
  testGroup
    "ConduitTest"
    [ TestDB.withOrvilleRun $ \run -> do
        testCase "selectConduit can read all result rows" $ do
          actual <-
            run $ do
              TestDB.reset schema
              O.insertRecordMany virusTable [bpsVirus, brnVirus]
              runResourceT $
                runConduit $
                fuse
                  (OC.selectConduit
                     (OS.selectQueryTable
                        virusTable
                        (O.order virusIdField O.Ascending)))
                  consume
          assertEqual
            "Expected all viruses to be loaded by conduit"
            [bpsVirus, brnVirus]
            actual

#if MIN_VERSION_conduit(1,3,0)
    , TestDB.withOrvilleRun $ \run -> do
        testCase "streamPages can read all result rows" $ do
          actual <-
            run $ do
              TestDB.reset schema
              O.insertRecordMany virusTable [bpsVirus, brnVirus]
              runResourceT $
                runConduit $
                fuse
                  (OC.streamPages
                     virusTable
                     virusIdField
                     virusId
                     Nothing
                     20)
                  consume
          assertEqual
            "Expected all viruses to be loaded by conduit"
            [bpsVirus, brnVirus]
            actual
#endif
    ]
