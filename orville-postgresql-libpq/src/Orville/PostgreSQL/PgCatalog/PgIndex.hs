module Orville.PostgreSQL.PgCatalog.PgIndex
  ( PgIndex (..),
    pgIndexTable,
    indexRelationOidField,
    indexIsLiveField,
  )
where

import qualified Data.Attoparsec.Text as AttoText
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidTypeField)
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeNumber, attributeNumberParser, attributeNumberTextBuilder)

{- |
  The Haskell representation of data read from the @pg_catalog.pg_index@ tale.
  Rows in this table contain extended information about indices. Information
  about indices is also contained in the @pg_catalog.pg_class@ table as well.
-}
data PgIndex = PgIndex
  { -- | The PostgreSQL @oid@ of the @pg_class@ entry for this index.
    pgIndexPgClassOid :: LibPQ.Oid
  , -- | The PostgreSQL @oid@ of the @pg_class@ entry for the table that this
    -- index is for.
    pgIndexRelationOid :: LibPQ.Oid
  , -- | An array of attribute numbers references the columns the table that
    -- are included in the index. An attribute number of @0@ indicates an
    -- expression over the table's columns rather than just a reference to a
    -- column.
    --
    -- In PostgreSQL 11+ this includes both key columns and non-key
    -- included columns. Orville is currently not aware of this distinction,
    -- however.
    pgIndexAttributeNumbers :: [AttributeNumber]
  , -- | Indicates whether this is a unique index
    pgIndexIsUnique :: Bool
  , -- | Indicates whether this is the primary key index for the table
    pgIndexIsPrimary :: Bool
  , -- | When @False@, indicates that this index is in the process of being dropped and should be ignored
    pgIndexIsLive :: Bool
  }

{- |
  An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_index@ table
-}
pgIndexTable :: Orville.TableDefinition Orville.NoKey PgIndex PgIndex
pgIndexTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey
      "pg_index"
      pgIndexMarshaller

pgIndexMarshaller :: Orville.SqlMarshaller PgIndex PgIndex
pgIndexMarshaller =
  PgIndex
    <$> Orville.marshallField pgIndexPgClassOid indexPgClassOidField
    <*> Orville.marshallField pgIndexRelationOid indexRelationOidField
    <*> Orville.marshallField pgIndexAttributeNumbers indexAttributeNumbersField
    <*> Orville.marshallField pgIndexIsUnique indexIsUniqueField
    <*> Orville.marshallField pgIndexIsPrimary indexIsPrimaryField
    <*> Orville.marshallField pgIndexIsLive indexIsLiveField

{- |
  The @indexrelid@ column of the @pg_index@ table
-}
indexPgClassOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
indexPgClassOidField =
  oidTypeField "indexrelid"

{- |
  The @indrelid@ column of the @pg_index@ table
-}
indexRelationOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
indexRelationOidField =
  oidTypeField "indrelid"

{- |
  The @indkey@ column of the @pg_index@ table
-}
indexAttributeNumbersField :: Orville.FieldDefinition Orville.NotNull [AttributeNumber]
indexAttributeNumbersField =
  Orville.convertField
    (Orville.tryConvertSqlType attributeNumberListToPgVectorText pgVectorTextToAttributeNumberList)
    (Orville.unboundedTextField "indkey")

{- |
  The @indisunique@ column of the @pg_index@ table
-}
indexIsUniqueField :: Orville.FieldDefinition Orville.NotNull Bool
indexIsUniqueField =
  Orville.booleanField "indisunique"

{- |
  The @indisprimary@ column of the @pg_index@ table
-}
indexIsPrimaryField :: Orville.FieldDefinition Orville.NotNull Bool
indexIsPrimaryField =
  Orville.booleanField "indisprimary"

{- |
  The @indislive@ column of the @pg_index@ table
-}
indexIsLiveField :: Orville.FieldDefinition Orville.NotNull Bool
indexIsLiveField =
  Orville.booleanField "indislive"

pgVectorTextToAttributeNumberList :: T.Text -> Either String [AttributeNumber]
pgVectorTextToAttributeNumberList text =
  let parser = do
        attNums <- AttoText.sepBy attributeNumberParser (AttoText.char ' ')
        AttoText.endOfInput
        pure attNums
   in case AttoText.parseOnly parser text of
        Left err -> Left ("Unable to decode PostgreSQL Vector as AttributeNumber list: " <> err)
        Right nums -> Right nums

attributeNumberListToPgVectorText :: [AttributeNumber] -> T.Text
attributeNumberListToPgVectorText attNums =
  let spaceDelimitedAttributeNumbers =
        mconcat $
          List.intersperse (LTB.singleton ' ') (map attributeNumberTextBuilder attNums)
   in LT.toStrict . LTB.toLazyText $
        spaceDelimitedAttributeNumbers
