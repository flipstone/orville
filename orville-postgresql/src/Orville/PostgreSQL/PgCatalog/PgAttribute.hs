{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgAttribute
  ( PgAttribute (..)
  , pgAttributeMaxLength
  , AttributeName
  , attributeNameToString
  , AttributeNumber
  , attributeNumberToInt16
  , attributeNumberFromInt16
  , attributeNumberTextBuilder
  , attributeNumberParser
  , isOrdinaryColumn
  , pgAttributeTable
  , attributeRelationOidField
  , attributeNameField
  , attributeTypeOidField
  , attributeLengthField
  , attributeIsDroppedField
  , attributeNumberTypeField
  )
where

import qualified Data.Attoparsec.Text as AttoText
import Data.Int (Int16, Int32)
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Marshall as Marshall
import Orville.PostgreSQL.PgCatalog.OidField (oidTypeField)

{- | The Haskell representation of data read from the @pg_catalog.pg_attribute@
  table. Rows in this table correspond to table columns, but also to attributes
  of other items from the @pg_class@ table.

  See also 'Orville.PostgreSQL.PgCatalog.PgClass'.

@since 1.0.0.0
-}
data PgAttribute = PgAttribute
  { pgAttributeRelationOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the relation that this
  -- attribute belongs to. References @pg_class.oid@.
  , pgAttributeName :: AttributeName
  -- ^ The name of the attribute.
  , pgAttributeNumber :: AttributeNumber
  -- ^ The PostgreSQL number of the attribute.
  , pgAttributeTypeOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the type of this attribute. References
  -- @pg_type.oid@.
  , pgAttributeLength :: Int16
  -- ^ The length of this attribute\'s type (a copy of @pg_type.typlen@). Note
  -- that this is _NOT_ the maximum length of a @varchar@ column!
  , pgAttributeTypeModifier :: Int32
  -- ^ Type-specific data supplied at creation time, such as the maximum length
  -- of a @varchar@ column.
  , pgAttributeIdentity :: Maybe Marshall.FieldIdentityGeneration
  -- ^ Indicates whether the column is an identity and how it is generated if at all.
  , pgAttributeIsDropped :: Bool
  -- ^ Indicates whether the column has been dropped and is not longer valid.
  , pgAttributeIsNotNull :: Bool
  -- ^ Indicates whether the column has a not-null constraint.
  }

{- | Returns the maximum length for an attribute with a variable length type,
  or 'Nothing' if the length of the type is not variable.

@since 1.0.0.0
-}
pgAttributeMaxLength :: PgAttribute -> Maybe Int32
pgAttributeMaxLength attr =
  -- This function is a port of the following function from _pg_char_max_length
  -- function postgresql:
  --
  -- Note that it does not handle DOMAIN (user created) types correctly at the
  -- moment. Handling domain types would require loading the pg_type record and
  -- checking whether to use the typid and typmod from the attribute or the
  -- base type and typemod from the domain type.
  --
  let
    charTypes =
      [LibPQ.Oid 1042, LibPQ.Oid 1043] -- char, varchar
    bitTypes =
      [LibPQ.Oid 1560, LibPQ.Oid 1562] -- bit, varbit
    typeOid =
      pgAttributeTypeOid attr

    typeMod =
      pgAttributeTypeModifier attr
  in
    if typeMod == -1
      then Nothing
      else
        if typeOid `elem` charTypes
          then Just (typeMod - 4)
          else
            if typeOid `elem` bitTypes
              then Just typeMod
              else Nothing

{- | Determines whether the attribute represents a system column by inspecting
  the attribute\'s 'AttributeNumber'. Ordinary columns have attribute numbers
  starting at 1.

@since 1.0.0.0
-}
isOrdinaryColumn :: PgAttribute -> Bool
isOrdinaryColumn attr =
  pgAttributeNumber attr > AttributeNumber 0

{- | A Haskell type for the name of the attribute represented by a 'PgAttribute'.

@since 1.0.0.0
-}
newtype AttributeName
  = AttributeName T.Text
  deriving
    ( -- | @since 1.0.0.0
      Show
    , -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Ord
    , -- | @since 1.0.0.0
      String.IsString
    )

{- | Converts an 'AttributeName' to a plain 'String'.

@since 1.0.0.0
-}
attributeNameToString :: AttributeName -> String
attributeNameToString (AttributeName txt) =
  T.unpack txt

{- | A Haskell type for the number of the attribute represented by a 'PgAttribute'.

@since 1.0.0.0
-}
newtype AttributeNumber
  = AttributeNumber Int16
  deriving
    ( -- | @since 1.0.0.0
      Show
    , -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Ord
    , -- | @since 1.0.0.0
      Enum
    , -- | @since 1.0.0.0
      Num
    , -- | @since 1.0.0.0
      Integral
    , -- | @since 1.0.0.0
      Real
    )

{- | Converts an 'AttributeNumber' to an integer.

@since 1.0.0.0
-}
attributeNumberToInt16 :: AttributeNumber -> Int16
attributeNumberToInt16 (AttributeNumber int) = int

{- | Converts an integer to an 'AttributeNumber'.

@since 1.0.0.0
-}
attributeNumberFromInt16 :: Int16 -> AttributeNumber
attributeNumberFromInt16 = AttributeNumber

{- | Attoparsec parser for 'AttributeNumber'.

@since 1.0.0.0
-}
attributeNumberParser :: AttoText.Parser AttributeNumber
attributeNumberParser =
  AttoText.signed AttoText.decimal

{- | Encodes an 'AttributeNumber' to lazy text as a builder.

@since 1.0.0.0
-}
attributeNumberTextBuilder :: AttributeNumber -> LTB.Builder
attributeNumberTextBuilder =
  LTBI.decimal . attributeNumberToInt16

{- | An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
pgAttributeTable :: Orville.TableDefinition Orville.NoKey PgAttribute PgAttribute
pgAttributeTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey
      "pg_attribute"
      pgAttributeMarshaller

pgAttributeMarshaller :: Orville.SqlMarshaller PgAttribute PgAttribute
pgAttributeMarshaller =
  PgAttribute
    <$> Orville.marshallField pgAttributeRelationOid attributeRelationOidField
    <*> Orville.marshallField pgAttributeName attributeNameField
    <*> Orville.marshallField pgAttributeNumber attributeNumberField
    <*> Orville.marshallField pgAttributeTypeOid attributeTypeOidField
    <*> Orville.marshallField pgAttributeLength attributeLengthField
    <*> Orville.marshallField pgAttributeTypeModifier attributeTypeModifierField
    <*> Orville.marshallField pgAttributeIdentity attributeIdentityField
    <*> Orville.marshallField pgAttributeIsDropped attributeIsDroppedField
    <*> Orville.marshallField pgAttributeIsNotNull attributeIsNotNullField

{- | The @attrelid@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeRelationOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
attributeRelationOidField =
  oidTypeField "attrelid"

{- | The @attname@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeNameField :: Orville.FieldDefinition Orville.NotNull AttributeName
attributeNameField =
  Orville.coerceField $
    Orville.unboundedTextField "attname"

{- | The @attnum@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeNumberField :: Orville.FieldDefinition Orville.NotNull AttributeNumber
attributeNumberField =
  attributeNumberTypeField "attnum"

{- | Builds a 'Orville.FieldDefinition' for a field with type 'AttributeNumber'.

@since 1.0.0.0
-}
attributeNumberTypeField :: String -> Orville.FieldDefinition Orville.NotNull AttributeNumber
attributeNumberTypeField =
  Orville.coerceField . Orville.smallIntegerField

{- | The @atttypid@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeTypeOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
attributeTypeOidField =
  oidTypeField "atttypid"

{- | The @attlen@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeLengthField :: Orville.FieldDefinition Orville.NotNull Int16
attributeLengthField =
  Orville.smallIntegerField "attlen"

{- | The @atttypmod@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeTypeModifierField :: Orville.FieldDefinition Orville.NotNull Int32
attributeTypeModifierField =
  Orville.integerField "atttypmod"

{- | The @attidentity@ column of the @pg_catalog.pg_attribute@ table.

@since 1.1.0.0
-}
attributeIdentityField ::
  Orville.FieldDefinition
    Orville.NotNull
    (Maybe Marshall.FieldIdentityGeneration)
attributeIdentityField =
  Orville.convertField
    (Orville.tryConvertSqlType columnIdentityToPgText pgTextToColumnIdentity)
    (Orville.fixedTextField "attidentity" 1)

{- | The @attisdropped@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeIsDroppedField :: Orville.FieldDefinition Orville.NotNull Bool
attributeIsDroppedField =
  Orville.booleanField "attisdropped"

{- | The @attnotnull@ column of the @pg_catalog.pg_attribute@ table.

@since 1.0.0.0
-}
attributeIsNotNullField :: Orville.FieldDefinition Orville.NotNull Bool
attributeIsNotNullField =
  Orville.booleanField "attnotnull"

{- | Internal, convert a 'T.Text' to a 'Maybe Marshall.FieldIdentityGeneration', allowing for the empty text case
   to be Nothing.

@since 1.1.0.0
-}
pgTextToColumnIdentity :: T.Text -> Either String (Maybe Marshall.FieldIdentityGeneration)
pgTextToColumnIdentity text =
  case T.unpack text of
    "" -> Right Nothing
    "a" -> Right (Just Marshall.GeneratedAlways)
    "d" -> Right (Just Marshall.GeneratedByDefault)
    attid -> Left ("Unrecognized PostgreSQL attribute identity: " <> attid)

{- | Internal, convert a 'Maybe Expr.ColumnIdentity' to a 'T.Text', allowing for the Nothing case to
   be the empty text.

@since 1.1.0.0
-}
columnIdentityToPgText :: Maybe Marshall.FieldIdentityGeneration -> T.Text
columnIdentityToPgText attid =
  T.pack $ case attid of
    Nothing -> ""
    (Just Marshall.GeneratedAlways) -> "a"
    (Just Marshall.GeneratedByDefault) -> "d"
