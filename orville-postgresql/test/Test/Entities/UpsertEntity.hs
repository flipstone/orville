module Test.Entities.UpsertEntity
  ( UpsertEntity (..)
  , UpsertEntityId
  , UpsertEntityCode
  , UpsertEntityValue
  , table
  , generate
  , generateUpsertEntityId
  , generateUpsertEntityCode
  , generateUpsertEntityValue
  , generateList
  , generateNonEmpty
  , withTable
  , upsertEntityIdField
  , upsertEntityCodeField
  , upsertEntityGroupAField
  , upsertEntityGroupBField
  , upsertEntityValueField
  , codeUniqueConstraint
  , groupUniqueConstraint
  , groupMarshaller
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Raw.Connection as Conn

import qualified Test.PgGen as PgGen
import qualified Test.TestTable as TestTable

type UpsertEntityId = Int32
type UpsertEntityCode = T.Text
type UpsertEntityValue = Int32

data UpsertEntity = UpsertEntity
  { upsertEntityId :: UpsertEntityId
  , upsertEntityCode :: UpsertEntityCode
  , upsertEntityGroupA :: T.Text
  , upsertEntityGroupB :: T.Text
  , upsertEntityValue :: UpsertEntityValue
  }
  deriving (Eq, Show)

-- Wrapper with special Eq and Ord instances so we can more easily generate unique UpsertEntities
newtype UniqueUpsertEntity = UniqueUpsertEntity {toUpsertEntity :: UpsertEntity}

instance Eq UniqueUpsertEntity where
  (UniqueUpsertEntity a) == (UniqueUpsertEntity b) =
    upsertEntityId a == upsertEntityId b
      || upsertEntityCode a == upsertEntityCode b
      || upsertEntityGroup a == upsertEntityGroup b

instance Ord UniqueUpsertEntity where
  compare ua@(UniqueUpsertEntity a) ub@(UniqueUpsertEntity b)
    | ua == ub = EQ
    | otherwise =
        compare (upsertEntityId a) (upsertEntityId b)
          <> compare (upsertEntityCode a) (upsertEntityCode b)
          <> compare (upsertEntityGroup a) (upsertEntityGroup b)

upsertEntityGroup :: UpsertEntity -> (T.Text, T.Text)
upsertEntityGroup e = (upsertEntityGroupA e, upsertEntityGroupB e)

table :: Orville.TableDefinition (Orville.HasKey UpsertEntityId) UpsertEntity UpsertEntity
table =
  Orville.addTableConstraints
    [codeUniqueConstraint, groupUniqueConstraint]
    $ Orville.mkTableDefinition "upsert_entity" (Orville.primaryKey upsertEntityIdField) upsertEntityMarshaller

upsertEntityMarshaller :: Orville.SqlMarshaller UpsertEntity UpsertEntity
upsertEntityMarshaller =
  UpsertEntity
    <$> Orville.marshallField upsertEntityId upsertEntityIdField
    <*> Orville.marshallField upsertEntityCode upsertEntityCodeField
    <*> Orville.marshallField upsertEntityGroupA upsertEntityGroupAField
    <*> Orville.marshallField upsertEntityGroupB upsertEntityGroupBField
    <*> Orville.marshallField upsertEntityValue upsertEntityValueField

upsertEntityIdField :: Orville.FieldDefinition Orville.NotNull UpsertEntityId
upsertEntityIdField =
  Orville.integerField "id"

upsertEntityCodeField :: Orville.FieldDefinition Orville.NotNull UpsertEntityCode
upsertEntityCodeField =
  Orville.unboundedTextField "code"

upsertEntityGroupAField :: Orville.FieldDefinition Orville.NotNull T.Text
upsertEntityGroupAField =
  Orville.unboundedTextField "group_a"

upsertEntityGroupBField :: Orville.FieldDefinition Orville.NotNull T.Text
upsertEntityGroupBField =
  Orville.unboundedTextField "group_b"

upsertEntityValueField :: Orville.FieldDefinition Orville.NotNull UpsertEntityValue
upsertEntityValueField =
  Orville.integerField "value"

codeUniqueConstraint :: Orville.ConstraintDefinition
codeUniqueConstraint =
  Orville.uniqueConstraint (Orville.fieldName upsertEntityCodeField :| [])

groupUniqueConstraint :: Orville.ConstraintDefinition
groupUniqueConstraint =
  Orville.uniqueConstraint (Orville.fieldName upsertEntityGroupAField :| [Orville.fieldName upsertEntityGroupBField])

groupMarshaller :: Orville.SqlMarshaller (T.Text, T.Text) (T.Text, T.Text)
groupMarshaller =
  (,)
    <$> Orville.marshallField fst upsertEntityGroupAField
    <*> Orville.marshallField snd upsertEntityGroupBField

generate :: HH.Gen UpsertEntity
generate =
  UpsertEntity
    <$> generateUpsertEntityId
    <*> generateUpsertEntityCode
    <*> generateGroupA
    <*> generateGroupB
    <*> generateUpsertEntityValue

generateUpsertEntityId :: HH.Gen UpsertEntityId
generateUpsertEntityId =
  PgGen.pgInt32

generateUpsertEntityCode :: HH.Gen UpsertEntityCode
generateUpsertEntityCode =
  PgGen.pgText (Range.constant 1 10)

generateGroupA :: HH.Gen T.Text
generateGroupA =
  PgGen.pgText (Range.constant 1 10)

generateGroupB :: HH.Gen T.Text
generateGroupB =
  PgGen.pgText (Range.constant 1 10)

generateUpsertEntityValue :: HH.Gen UpsertEntityValue
generateUpsertEntityValue =
  Gen.integral (Range.constant 0 100)

generateList :: HH.Range Int -> HH.Gen [UpsertEntity]
generateList range =
  fmap (fmap toUpsertEntity . Set.toList) (Gen.set range (fmap UniqueUpsertEntity generate))

generateNonEmpty :: HH.Range Int -> HH.Gen (NEL.NonEmpty UpsertEntity)
generateNonEmpty range = do
  set <- Gen.set range (fmap UniqueUpsertEntity generate)
  case fmap toUpsertEntity (Set.toList set) of
    (x : xs) -> pure (x :| xs)
    [] -> fmap (:| []) generate

withTable :: MonadIO m => Orville.ConnectionPool -> Orville.Orville a -> m a
withTable pool operation =
  liftIO $ do
    Conn.withPoolConnection pool $ \connection ->
      TestTable.dropAndRecreateTableDef connection table
    Orville.runOrville pool operation
