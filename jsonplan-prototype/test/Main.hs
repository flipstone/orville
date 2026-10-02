module Main
  ( main
  ) where

import qualified Control.Monad as Monad
import qualified Hedgehog as HH
import qualified System.Exit as Exit

import qualified Orville.PostgreSQL as O

import qualified Test.CompileChecks as CompileChecks
import qualified Test.Decoders as Decoders
import qualified Test.Equivalence as Equivalence
import qualified Test.Fixtures as Fixtures
import qualified Test.WireTextLaw as WireTextLaw

main :: IO ()
main = do
  pool <- Fixtures.createTestPool
  O.runOrville pool Fixtures.createSchema
  results <-
    traverse
      HH.checkSequential
      [ WireTextLaw.tests pool
      , Equivalence.tests pool
      , CompileChecks.tests
      , Decoders.tests pool
      ]
  Monad.unless (and results) Exit.exitFailure
