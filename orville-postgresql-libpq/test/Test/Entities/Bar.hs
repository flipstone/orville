module Test.Entities.Bar
  ( Bar (..)
  , table
  , generate
  , generateList
  , withTable
  , barIdField
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville

import qualified Test.PgGen as PgGen
import qualified Test.TestTable as TestTable

type BarId = Int32
type BarName = T.Text

data Bar barId = Bar
  { barId :: barId
  , barName :: BarName
  }
  deriving (Eq, Show)

type BarWrite = Bar ()
type BarRead = Bar BarId

table :: Orville.TableDefinition (Orville.HasKey BarId) BarWrite BarRead
table =
  Orville.mkTableDefinition "bar" (Orville.primaryKey barIdField) barMarshaller

barMarshaller :: Orville.SqlMarshaller BarWrite BarRead
barMarshaller =
  Bar
    <$> Orville.marshallReadOnly (Orville.marshallField barId barIdField)
    <*> Orville.marshallField barName barNameField

barIdField :: Orville.FieldDefinition Orville.NotNull BarId
barIdField =
  Orville.serialField "id"

barNameField :: Orville.FieldDefinition Orville.NotNull BarName
barNameField =
  Orville.unboundedTextField "name"

generate :: HH.Gen BarWrite
generate =
  Bar ()
    <$> PgGen.pgText (Range.constant 0 10)

generateList :: HH.Range Int -> HH.Gen [BarWrite]
generateList range =
  (Gen.list range generate)

withTable :: MonadIO m => Pool Orville.Connection -> Orville.Orville a -> m a
withTable pool operation =
  liftIO $ do
    withResource pool $ \connection ->
      TestTable.dropAndRecreateTableDef connection table
    Orville.runOrville pool operation
