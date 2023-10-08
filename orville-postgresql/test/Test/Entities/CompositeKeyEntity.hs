module Test.Entities.CompositeKeyEntity
  ( CompositeKey (..)
  , CompositeKeyEntity (..)
  , CompositeKeyEntityId
  , CompositeKeyEntityName
  , table
  , generate
  , generateCompositeKeyEntityId
  , generateCompositeKeyEntityName
  , generateNonEmpty
  , withTable
  , compositeKeyEntityIdField
  , compositeKeyEntityNameField
  , compositeKeyEntityAgeField
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function (on)
import Data.Int (Int32)
import qualified Data.List.NonEmpty as NEL
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville

import qualified Test.PgGen as PgGen
import qualified Test.TestTable as TestTable

type CompositeKeyEntityId = Int32
type CompositeKeyEntityName = T.Text
type CompositeKeyEntityAge = Int32

data CompositeKey = CompositeKey
  { compositeKeyEntityId :: CompositeKeyEntityId
  , compositeKeyEntityName :: CompositeKeyEntityName
  }
  deriving (Eq, Ord, Show)

data CompositeKeyEntity = CompositeKeyEntity
  { compositeKey :: CompositeKey
  , compositeKeyEntityAge :: CompositeKeyEntityAge
  }
  deriving (Eq, Show)

table :: Orville.TableDefinition (Orville.HasKey CompositeKey) CompositeKeyEntity CompositeKeyEntity
table =
  Orville.mkTableDefinition "compositeKeyEntity" primaryKey compositeKeyEntityMarshaller

primaryKey :: Orville.PrimaryKey CompositeKey
primaryKey =
  Orville.compositePrimaryKey
    (Orville.primaryKeyPart compositeKeyEntityId compositeKeyEntityIdField)
    [Orville.primaryKeyPart compositeKeyEntityName compositeKeyEntityNameField]

compositeKeyEntityMarshaller :: Orville.SqlMarshaller CompositeKeyEntity CompositeKeyEntity
compositeKeyEntityMarshaller =
  CompositeKeyEntity
    <$> Orville.marshallNested compositeKey compositeKeyMap
    <*> Orville.marshallField compositeKeyEntityAge compositeKeyEntityAgeField

compositeKeyMap :: Orville.SqlMarshaller CompositeKey CompositeKey
compositeKeyMap =
  CompositeKey
    <$> Orville.marshallField compositeKeyEntityId compositeKeyEntityIdField
    <*> Orville.marshallField compositeKeyEntityName compositeKeyEntityNameField

compositeKeyEntityIdField :: Orville.FieldDefinition Orville.NotNull CompositeKeyEntityId
compositeKeyEntityIdField =
  Orville.integerField "id"

compositeKeyEntityNameField :: Orville.FieldDefinition Orville.NotNull CompositeKeyEntityName
compositeKeyEntityNameField =
  Orville.unboundedTextField "name"

compositeKeyEntityAgeField :: Orville.FieldDefinition Orville.NotNull CompositeKeyEntityAge
compositeKeyEntityAgeField =
  Orville.integerField "age"

generate :: HH.Gen CompositeKeyEntity
generate =
  CompositeKeyEntity
    <$> ( CompositeKey
            <$> generateCompositeKeyEntityId
            <*> generateCompositeKeyEntityName
        )
    <*> generateCompositeKeyEntityAge

generateCompositeKeyEntityId :: HH.Gen CompositeKeyEntityId
generateCompositeKeyEntityId =
  PgGen.pgInt32

generateCompositeKeyEntityName :: HH.Gen CompositeKeyEntityName
generateCompositeKeyEntityName =
  PgGen.pgText (Range.constant 0 10)

generateCompositeKeyEntityAge :: HH.Gen CompositeKeyEntityAge
generateCompositeKeyEntityAge =
  Gen.integral (Range.constant minCompositeKeyEntityAge maxCompositeKeyEntityAge)

minCompositeKeyEntityAge :: CompositeKeyEntityAge
minCompositeKeyEntityAge = 0

maxCompositeKeyEntityAge :: CompositeKeyEntityAge
maxCompositeKeyEntityAge = 50

generateNonEmpty :: HH.Range Int -> HH.Gen (NEL.NonEmpty CompositeKeyEntity)
generateNonEmpty range =
  fmap
    (NEL.nubBy ((==) `on` compositeKey))
    (Gen.nonEmpty range generate)

withTable :: MonadIO m => Pool Orville.Connection -> Orville.Orville a -> m a
withTable pool operation =
  liftIO $ do
    withResource pool $ \connection ->
      TestTable.dropAndRecreateTableDef connection table
    Orville.runOrville pool operation
