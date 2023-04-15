module Test.PostgreSQLAxioms
  ( postgreSQLAxiomTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.Property as Property

postgreSQLAxiomTests :: Orville.Pool Orville.Connection -> Property.Group
postgreSQLAxiomTests pool =
  Property.group
    "PostgreSQL Axioms"
    [ prop_smallIntegerBounds pool
    , prop_integerBounds pool
    , prop_bigIntegerBounds pool
    ]

prop_smallIntegerBounds :: Property.NamedDBProperty
prop_smallIntegerBounds =
  Property.singletonNamedDBProperty "smallinteger bounds match Haskell Int16" $ \pool -> do
    assertPostgreSQLMinValueMatchesHaskell pool SqlType.smallInteger
    assertPostgreSQLMaxValueMatchesHaskell pool SqlType.smallInteger

prop_integerBounds :: Property.NamedDBProperty
prop_integerBounds =
  Property.singletonNamedDBProperty "integer bounds match Haskell Int32" $ \pool -> do
    assertPostgreSQLMinValueMatchesHaskell pool SqlType.integer
    assertPostgreSQLMaxValueMatchesHaskell pool SqlType.integer

prop_bigIntegerBounds :: Property.NamedDBProperty
prop_bigIntegerBounds =
  Property.singletonNamedDBProperty "bigInteger bounds match Haskell Int64" $ \pool -> do
    assertPostgreSQLMinValueMatchesHaskell pool SqlType.bigInteger
    assertPostgreSQLMaxValueMatchesHaskell pool SqlType.bigInteger

assertPostgreSQLMinValueMatchesHaskell ::
  (Eq a, Show a, Bounded a, HH.MonadTest m, MIO.MonadIO m, ExSafe.MonadCatch m) =>
  Orville.Pool Orville.Connection ->
  SqlType.SqlType a ->
  m ()
assertPostgreSQLMinValueMatchesHaskell pool sqlType = do
  -- First check that the min value can be selected via PostgreSQL
  result <- HH.evalEitherM $ selectValue pool minBound sqlType
  result === minBound

  -- Then check that minvalue - 1 gives a numeric overflow error, verifying that
  -- the min value haskell is, in fact, the minimum value in PostgreSQL
  err <- evalEitherMLeft $ selectValueWithModifier pool minBound (RawSql.fromString "-1") sqlType
  Conn.sqlExecutionErrorSqlState err === Just (B8.pack "22003")

assertPostgreSQLMaxValueMatchesHaskell ::
  (Eq a, Show a, Bounded a, HH.MonadTest m, MIO.MonadIO m, ExSafe.MonadCatch m) =>
  Orville.Pool Orville.Connection ->
  SqlType.SqlType a ->
  m ()
assertPostgreSQLMaxValueMatchesHaskell pool sqlType = do
  -- First check that the max value can be selected via PostgreSQL
  result <- HH.evalEitherM $ selectValue pool maxBound sqlType
  result === maxBound

  -- Then check that maxvalue - 1 gives a numeric overflow error, verifying that
  -- the max value haskell is, in fact, the maximum value in PostgreSQL
  err <- evalEitherMLeft $ selectValueWithModifier pool maxBound (RawSql.fromString "+1") sqlType
  Conn.sqlExecutionErrorSqlState err === Just (B8.pack "22003")

evalEitherMLeft :: (HH.MonadTest m, ExSafe.MonadCatch m, Show b) => m (Either a b) -> m a
evalEitherMLeft =
  let swap :: Either a b -> Either b a
      swap (Left a) = Right a
      swap (Right b) = Left b
   in HH.evalEitherM . fmap swap

selectValue ::
  (HH.MonadTest m, MIO.MonadIO m) =>
  Orville.Pool Orville.Connection ->
  a ->
  SqlType.SqlType a ->
  m (Either Conn.SqlExecutionError a)
selectValue pool inputValue sqlType =
  selectValueWithModifier pool inputValue mempty sqlType

selectValueWithModifier ::
  (HH.MonadTest m, MIO.MonadIO m) =>
  Orville.Pool Orville.Connection ->
  a ->
  RawSql.RawSql ->
  SqlType.SqlType a ->
  m (Either Conn.SqlExecutionError a)
selectValueWithModifier pool inputValue modifier sqlType = do
  let selectOne =
        RawSql.fromString "SELECT (("
          <> RawSql.parameter (SqlType.sqlTypeToSql sqlType inputValue)
          <> modifier
          <> RawSql.fromString ")"
          <> RawSql.fromString "::"
          <> RawSql.toRawSql (SqlType.sqlTypeExpr sqlType)
          <> RawSql.fromString ") as result"

      marshaller =
        Orville.annotateSqlMarshallerEmptyAnnotation $
          Orville.marshallField id (Orville.fieldOfType sqlType "result")

  errOrResults <-
    HH.evalIO . Orville.runOrville pool . ExSafe.try $
      Orville.executeAndDecode Orville.SelectQuery selectOne marshaller

  case errOrResults of
    Left err ->
      pure (Left err)
    Right [outputValue] ->
      pure (Right outputValue)
    Right results -> do
      HH.annotate $ "Expected exactly one result row from query, but got " <> show (length results)
      HH.failure
