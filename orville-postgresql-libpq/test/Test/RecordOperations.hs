module Test.RecordOperations
  ( recordOperationsTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Connection

import qualified Test.Entities.Foo as Foo
import qualified Test.TestTable as TestTable

recordOperationsTests :: Pool.Pool Connection.Connection -> IO Bool
recordOperationsTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "RecordOperations")
      [
        ( String.fromString "insertRecord does not raise an error"
        , HH.property $ do
            foo <- HH.forAll Foo.generate

            MIO.liftIO $ do
              Pool.withResource pool $ \connection ->
                TestTable.dropAndRecreateTableDef connection Foo.table
              Orville.runOrville pool $ Orville.insertRecord Foo.table foo
        )
      ]
