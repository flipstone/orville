module Test.Expr.SequenceDefinition
  ( sequenceDefinitionTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.PgAssert as PgAssert
import qualified Test.Property as Property

sequenceDefinitionTests :: Orville.ConnectionPool -> Tasty.TestTree
sequenceDefinitionTests pool =
  Tasty.testGroup
    "Expr - SequenceDefinition"
    [ TastyHH.testProperty "Create sequence with no options" (prop_createWithNoOptions pool)
    , TastyHH.testProperty "Create sequence with options" (prop_createWithWithOptions pool)
    , TastyHH.testProperty "dropSequenceExpr renders the drop behavior" prop_dropSequenceSql
    ]

prop_dropSequenceSql :: HH.Property
prop_dropSequenceSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.dropSequenceExpr (Just Expr.ifExists) exprSequenceName (Just Expr.cascade))
      === B8.pack "DROP SEQUENCE IF EXISTS \"sequence_definition_test\" CASCADE"

prop_createWithNoOptions :: Orville.ConnectionPool -> HH.Property
prop_createWithNoOptions pool =
  Property.singletonProperty $ do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) exprSequenceName Nothing
        Orville.executeVoid Orville.DDLQuery $ Expr.createSequenceExpr exprSequenceName Nothing Nothing Nothing Nothing Nothing Nothing

    _ <- PgAssert.assertSequenceExists pool sequenceNameString
    pure ()

prop_createWithWithOptions :: Orville.ConnectionPool -> HH.Property
prop_createWithWithOptions pool =
  Property.singletonProperty $ do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) exprSequenceName Nothing
        Orville.executeVoid Orville.DDLQuery $
          Expr.createSequenceExpr
            exprSequenceName
            (Just $ Expr.incrementBy 2)
            (Just $ Expr.minValue 100)
            (Just $ Expr.maxValue 200)
            (Just $ Expr.startWith 107)
            (Just $ Expr.cache 10)
            (Just Expr.cycle)

    relation <- PgAssert.assertSequenceExists pool sequenceNameString
    pgSequence <- PgAssert.assertRelationHasPgSequence relation
    PgCatalog.pgSequenceIncrement pgSequence === 2
    PgCatalog.pgSequenceStart pgSequence === 107
    PgCatalog.pgSequenceMin pgSequence === 100
    PgCatalog.pgSequenceMax pgSequence === 200
    PgCatalog.pgSequenceCache pgSequence === 10
    PgCatalog.pgSequenceCycle pgSequence === True

exprSequenceName :: Expr.QualifiedOrUnqualified Expr.SequenceName
exprSequenceName =
  Expr.unqualified (Expr.sequenceName sequenceNameString)

sequenceNameString :: String
sequenceNameString =
  "sequence_definition_test"
