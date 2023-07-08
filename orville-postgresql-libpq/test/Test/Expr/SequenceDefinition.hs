module Test.Expr.SequenceDefinition
  ( sequenceDefinitionTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Hedgehog ((===))

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

import qualified Test.PgAssert as PgAssert
import qualified Test.Property as Property

sequenceDefinitionTests :: Orville.Pool Orville.Connection -> Property.Group
sequenceDefinitionTests pool =
  Property.group
    "Expr - SequenceDefinition"
    [ prop_createWithNoOptions pool
    , prop_createWithWithOptions pool
    ]

prop_createWithNoOptions :: Property.NamedDBProperty
prop_createWithNoOptions =
  Property.singletonNamedDBProperty "Create sequence with no options" $ \pool -> do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) exprSequenceName
        Orville.executeVoid Orville.DDLQuery $ Expr.createSequenceExpr exprSequenceName Nothing Nothing Nothing Nothing Nothing Nothing

    _ <- PgAssert.assertSequenceExists pool sequenceNameString
    pure ()

prop_createWithWithOptions :: Property.NamedDBProperty
prop_createWithWithOptions =
  Property.singletonNamedDBProperty "Create sequence with no options" $ \pool -> do
    MIO.liftIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) exprSequenceName
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

exprSequenceName :: Expr.Qualified Expr.SequenceName
exprSequenceName =
  Expr.qualified Nothing (Expr.sequenceName sequenceNameString)

sequenceNameString :: String
sequenceNameString =
  "sequence_definition_test"
