{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestDB where

import Control.Monad (void)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as Writer
import Data.Convertible (convert)
import Data.Pool (Pool, createPool, destroyAllResources)
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as Postgres
import System.Environment (getEnv)

import Test.Tasty (TestTree, withResource)

import qualified Database.Orville as O
import qualified Database.Orville.Raw as ORaw

type TestPool = Pool Postgres.Connection

type QueryWriterT = WriterT [(O.QueryType, String)]

newtype TestMonad a = TestMonad
  { runTestMonad :: QueryWriterT (O.OrvilleT Postgres.Connection IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO, MonadThrow)

queryTrace ::
     (O.QueryType -> Bool) -> TestMonad a -> TestMonad [(O.QueryType, String)]
queryTrace queryPred (TestMonad writer) = do
  (_, fullTrace) <- TestMonad (Writer.listen writer)
  pure (filter (queryPred . fst) fullTrace)

instance MonadBaseControl IO TestMonad where
  type StM TestMonad a = StM (QueryWriterT (O.OrvilleT Postgres.Connection IO)) a
  liftBaseWith f =
    TestMonad $ liftBaseWith $ \runInBase -> f (\(TestMonad m) -> runInBase m)
  restoreM stm = TestMonad (restoreM stm)

instance O.MonadOrville Postgres.Connection TestMonad where
  getOrvilleEnv = TestMonad (lift O.getOrvilleEnv)
  localOrvilleEnv f (TestMonad m) =
    TestMonad (Writer.mapWriterT (O.localOrvilleEnv f) m)
  runningQuery typ sql action = do
    TestMonad (Writer.tell [(typ, sql)])
    action

reset :: O.SchemaDefinition -> O.Orville ()
reset schemaDef = do
  results <- ORaw.selectSqlRows "SELECT current_user" []
  case results of
    [[("current_user", currentUser)]]
    -- I would like to use placeholders here, but postgres gives my a
    -- sql syntax error when I do :(
     -> void $ ORaw.updateSql ("DROP OWNED BY " ++ convert currentUser) []
    _ ->
      error $ "Expected single 'current_user' result row, got " ++ show results
  O.migrateSchema schemaDef

withOrvilleRun :: ((forall a. TestMonad a -> IO a) -> TestTree) -> TestTree
withOrvilleRun mkTree = withDb (\pool -> mkTree (run pool))
  where
    run :: IO TestPool -> forall a. TestMonad a -> IO a
    run getPool action = do
      pool <- getPool
      fmap
        fst
        (O.runOrville
           (Writer.runWriterT (runTestMonad action))
           (O.newOrvilleEnv pool))

withDb :: (IO TestPool -> TestTree) -> TestTree
withDb = withResource acquirePool destroyAllResources

acquirePool :: IO TestPool
acquirePool = do
  connString <- getEnv "TEST_CONN_STRING"
  createPool (Postgres.connectPostgreSQL' connString) HDBC.disconnect 1 60 1
