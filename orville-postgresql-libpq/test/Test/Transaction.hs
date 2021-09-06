module Test.Transaction
  ( transactionTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn

import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

transactionTests :: Pool.Pool Conn.Connection -> Property.Group
transactionTests pool =
  Property.group "Transaction" $
    [
      ( String.fromString "Transactions without exceptions perform a commit"
      , HH.property $ do
          nestingLevel <- HH.forAll genNestingLevel

          tracers <-
            MIO.liftIO $ do
              Pool.withResource pool $ \connection ->
                TestTable.dropAndRecreateTableDef connection tracerTable

              Orville.runOrville pool $ do
                withNestedTransactions nestingLevel $
                  NestedActions
                    { atEachLevel = Orville.insertEntity tracerTable Tracer
                    , atInnermost = pure ()
                    }

                Orville.findEntitiesBy tracerTable mempty

          length tracers === (nestingLevel + 1)
      )
    ,
      ( String.fromString "Exceptions within transaction blocks execute rollbock"
      , HH.property $ do
          nestingLevel <- HH.forAll genNestingLevel

          tracers <-
            MIO.liftIO $ do
              Pool.withResource pool $ \connection ->
                TestTable.dropAndRecreateTableDef connection tracerTable

              Orville.runOrville pool $ do
                ExSafe.handle (\TestError -> pure ()) $
                  withNestedTransactions nestingLevel $
                    NestedActions
                      { atEachLevel = Orville.insertEntity tracerTable Tracer
                      , atInnermost = ExSafe.throw TestError
                      }

                Orville.findEntitiesBy tracerTable mempty

          length tracers === 0
      )
    ,
      ( String.fromString "Savepoints allow inner transactions to rollback while outer transactions commit"
      , HH.property $ do
          outerNestingLevel <- HH.forAll genNestingLevel
          innerNestingLevel <- HH.forAll genNestingLevel

          let doInnerTransactions =
                ExSafe.handle (\TestError -> pure ()) $
                  withNestedTransactions innerNestingLevel $
                    NestedActions
                      { atEachLevel = Orville.insertEntity tracerTable Tracer
                      , atInnermost = ExSafe.throw TestError
                      }

              doTransactions =
                withNestedTransactions outerNestingLevel $
                  NestedActions
                    { atEachLevel = Orville.insertEntity tracerTable Tracer
                    , atInnermost = doInnerTransactions
                    }

          tracers <-
            MIO.liftIO $ do
              Pool.withResource pool $ \connection ->
                TestTable.dropAndRecreateTableDef connection tracerTable

              Orville.runOrville pool $ do
                doTransactions
                Orville.findEntitiesBy tracerTable mempty

          length tracers === (outerNestingLevel + 1)
      )
    ]

data NestedActions = NestedActions
  { atEachLevel :: Orville.Orville ()
  , atInnermost :: Orville.Orville ()
  }

withNestedTransactions ::
  Int ->
  NestedActions ->
  Orville.Orville ()
withNestedTransactions nestingLevel nestedActions =
  Orville.withTransaction $ do
    atEachLevel nestedActions
    if nestingLevel == 0
      then atInnermost nestedActions
      else withNestedTransactions (nestingLevel - 1) nestedActions

genNestingLevel :: HH.Gen Int
genNestingLevel =
  Gen.integral $ Range.linear 0 5

data Tracer
  = Tracer

tracerTable :: Orville.TableDefinition Orville.NoKey Tracer Tracer
tracerTable =
  Orville.mkTableDefinitionWithoutKey "tracer" tracerMarshaller

tracerMarshaller :: Orville.SqlMarshaller Tracer Tracer
tracerMarshaller =
  const Tracer
    <$> Orville.marshallField (const $ T.pack "tracer") (Orville.unboundedTextField "tracer")

data TestError
  = TestError
  deriving (Show)

instance ExSafe.Exception TestError
