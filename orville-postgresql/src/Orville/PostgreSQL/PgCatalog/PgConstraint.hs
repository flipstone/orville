{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgConstraint
  ( PgConstraint (..)
  , ConstraintType (..)
  , ConstraintName
  , constraintNameToString
  , pgConstraintTable
  , constraintRelationOidField
  )
where

import qualified Data.Attoparsec.Text as AttoText
import qualified Data.List as List
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField, oidTypeField)
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeNumber, attributeNumberParser, attributeNumberTextBuilder)

{- |
  The Haskell representation of data read from the @pg_catalog.pg_constraint@
  tale. Rows in this table correspond to check, primary key, unique, foreign
  key and exclusion constraints on tables.

@since 1.0.0.0
-}
data PgConstraint = PgConstraint
  { pgConstraintOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the constraint
  , pgConstraintName :: ConstraintName
  -- ^ The constraint name (which may not be unique)
  , pgConstraintNamespaceOid :: LibPQ.Oid
  -- ^ The oid of the namespace that contains the constraint
  , pgConstraintType :: ConstraintType
  -- ^ The type of constraint
  , pgConstraintRelationOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ of the table that the constraint is on
  -- (or @0@ if not a table constraint)
  , pgConstraintIndexOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ ef the index supporting this constraint, if it's a
  -- unique, primary key, foreign key or exclusion constraint. Otherwise @0@.
  , pgConstraintKey :: Maybe [AttributeNumber]
  -- ^ For table constraints, the attribute numbers of the constrained columns. These
  -- correspond to the 'pgAttributeNumber' field of 'PgAttribute'.
  , pgConstraintForeignRelationOid :: LibPQ.Oid
  -- ^ For foreign key constraints, the PostgreSQL @oid@ of the table the
  -- foreign key references
  , pgConstraintForeignKey :: Maybe [AttributeNumber]
  -- ^ For foreignkey constraints, the attribute numbers of the referenced columns. These
  -- correspond to the 'pgAttributeNumber' field of 'PgAttribute'.
  , pgConstraintForeignKeyOnUpdateType :: Maybe Orville.ForeignKeyAction
  -- ^ For foreignkey constraints, the on update action type
  , pgConstraintForeignKeyOnDeleteType :: Maybe Orville.ForeignKeyAction
  -- ^ For foreignkey constraints, the on delete action type
  }

{- |
  A Haskell type for the name of the constraint represented by a 'PgConstraint'

@since 1.0.0.0
-}
newtype ConstraintName
  = ConstraintName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  Converts an 'ConstraintName' to a plain old string

@since 1.0.0.0
-}
constraintNameToString :: ConstraintName -> String
constraintNameToString (ConstraintName txt) =
  T.unpack txt

{- |
  The type of constraint that a 'PgConstraint' represents, as described at
  https://www.postgresql.org/docs/13/catalog-pg-constraint.html

@since 1.0.0.0
-}
data ConstraintType
  = CheckConstraint
  | ForeignKeyConstraint
  | PrimaryKeyConstraint
  | UniqueConstraint
  | ConstraintTrigger
  | ExclusionConstraint
  deriving (Show, Eq)

{- |
  Converts a 'ConstraintType' to the corresponding single character text
  representation used by PostgreSQL.

  See also 'pgTextToConstraintType'

@since 1.0.0.0
-}
constraintTypeToPgText :: ConstraintType -> T.Text
constraintTypeToPgText conType =
  T.pack $
    case conType of
      CheckConstraint -> "c"
      ForeignKeyConstraint -> "f"
      PrimaryKeyConstraint -> "p"
      UniqueConstraint -> "u"
      ConstraintTrigger -> "t"
      ExclusionConstraint -> "x"

{- |
  Attempts to parse a PostgreSQL single character textual value as a
  'ConstraintType'

  See also 'constraintTypeToPgText'

@since 1.0.0.0
-}
pgTextToConstraintType :: T.Text -> Either String ConstraintType
pgTextToConstraintType text =
  case T.unpack text of
    "c" -> Right CheckConstraint
    "f" -> Right ForeignKeyConstraint
    "p" -> Right PrimaryKeyConstraint
    "u" -> Right UniqueConstraint
    "t" -> Right ConstraintTrigger
    "x" -> Right ExclusionConstraint
    typ -> Left ("Unrecognized PostgreSQL constraint type: " <> typ)

{- |
  Converts a 'Maybe Orville.ForeignKeyAction' to the corresponding single character
  text representation used by PostgreSQL.

  See also 'pgTextToForeignKeyAction'

@since 1.0.0.0
-}
foreignKeyActionToPgText :: Maybe Orville.ForeignKeyAction -> T.Text
foreignKeyActionToPgText mbfkAction =
  T.pack $
    case mbfkAction of
      Just Orville.NoAction -> "a"
      Just Orville.Restrict -> "r"
      Just Orville.Cascade -> "c"
      Just Orville.SetNull -> "n"
      Just Orville.SetDefault -> "d"
      Nothing -> " "

{- |
  Attempts to parse a PostgreSQL single character textual value as a
  'Maybe Orville.ForeignKeyAction'

  See also 'foreignKeyActionToPgText'

@since 1.0.0.0
-}
pgTextToForeignKeyAction :: T.Text -> Either String (Maybe Orville.ForeignKeyAction)
pgTextToForeignKeyAction text =
  case T.unpack text of
    "a" -> Right $ Just Orville.NoAction
    "r" -> Right $ Just Orville.Restrict
    "c" -> Right $ Just Orville.Cascade
    "n" -> Right $ Just Orville.SetNull
    "d" -> Right $ Just Orville.SetDefault
    " " -> Right Nothing
    typ -> Left ("Unrecognized PostgreSQL foreign key action type: " <> typ)

{- |
  An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_constraint@ table

@since 1.0.0.0
-}
pgConstraintTable :: Orville.TableDefinition (Orville.HasKey LibPQ.Oid) PgConstraint PgConstraint
pgConstraintTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinition
      "pg_constraint"
      (Orville.primaryKey oidField)
      pgConstraintMarshaller

pgConstraintMarshaller :: Orville.SqlMarshaller PgConstraint PgConstraint
pgConstraintMarshaller =
  PgConstraint
    <$> Orville.marshallField pgConstraintOid oidField
    <*> Orville.marshallField pgConstraintName constraintNameField
    <*> Orville.marshallField pgConstraintNamespaceOid constraintNamespaceOidField
    <*> Orville.marshallField pgConstraintType constraintTypeField
    <*> Orville.marshallField pgConstraintRelationOid constraintRelationOidField
    <*> Orville.marshallField pgConstraintIndexOid constraintIndexOidField
    <*> Orville.marshallField pgConstraintKey constraintKeyField
    <*> Orville.marshallField pgConstraintForeignRelationOid constraintForeignRelationOidField
    <*> Orville.marshallField pgConstraintForeignKey constraintForeignKeyField
    <*> Orville.marshallField pgConstraintForeignKeyOnUpdateType constraintForeignKeyOnUpdateTypeField
    <*> Orville.marshallField pgConstraintForeignKeyOnDeleteType constraintForeignKeyOnDeleteTypeField

{- |
  The @conname@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintNameField :: Orville.FieldDefinition Orville.NotNull ConstraintName
constraintNameField =
  Orville.coerceField $
    Orville.unboundedTextField "conname"

{- |
  The @connamespace@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintNamespaceOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
constraintNamespaceOidField =
  oidTypeField "connamespace"

{- |
  The @contype@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintTypeField :: Orville.FieldDefinition Orville.NotNull ConstraintType
constraintTypeField =
  Orville.convertField
    (Orville.tryConvertSqlType constraintTypeToPgText pgTextToConstraintType)
    (Orville.unboundedTextField "contype")

{- |
  The @conrelid@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintRelationOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
constraintRelationOidField =
  oidTypeField "conrelid"

{- |
  The @conindid@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintIndexOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
constraintIndexOidField =
  oidTypeField "conindid"

{- |
  The @conkey@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintKeyField :: Orville.FieldDefinition Orville.Nullable (Maybe [AttributeNumber])
constraintKeyField =
  Orville.nullableField $
    Orville.convertField
      (Orville.tryConvertSqlType attributeNumberListToPgArrayText pgArrayTextToAttributeNumberList)
      (Orville.unboundedTextField "conkey")

{- |
  The @confrelid@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintForeignRelationOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
constraintForeignRelationOidField =
  oidTypeField "confrelid"

{- |
  The @confkey@ column of the @pg_constraint@ table

@since 1.0.0.0
-}
constraintForeignKeyField :: Orville.FieldDefinition Orville.Nullable (Maybe [AttributeNumber])
constraintForeignKeyField =
  Orville.nullableField $
    Orville.convertField
      (Orville.tryConvertSqlType attributeNumberListToPgArrayText pgArrayTextToAttributeNumberList)
      (Orville.unboundedTextField "confkey")

constraintForeignKeyOnUpdateTypeField :: Orville.FieldDefinition Orville.NotNull (Maybe Orville.ForeignKeyAction)
constraintForeignKeyOnUpdateTypeField =
  Orville.convertField
    (Orville.tryConvertSqlType foreignKeyActionToPgText pgTextToForeignKeyAction)
    (Orville.unboundedTextField "confupdtype")

constraintForeignKeyOnDeleteTypeField :: Orville.FieldDefinition Orville.NotNull (Maybe Orville.ForeignKeyAction)
constraintForeignKeyOnDeleteTypeField =
  Orville.convertField
    (Orville.tryConvertSqlType foreignKeyActionToPgText pgTextToForeignKeyAction)
    (Orville.unboundedTextField "confdeltype")

pgArrayTextToAttributeNumberList :: T.Text -> Either String [AttributeNumber]
pgArrayTextToAttributeNumberList text =
  let
    parser = do
      _ <- AttoText.char '{'
      attNums <- AttoText.sepBy attributeNumberParser (AttoText.char ',')
      _ <- AttoText.char '}'
      AttoText.endOfInput
      pure attNums
  in
    case AttoText.parseOnly parser text of
      Left err -> Left ("Unable to decode PostgreSQL Array as AttributeNumber list: " <> err)
      Right nums -> Right nums

attributeNumberListToPgArrayText :: [AttributeNumber] -> T.Text
attributeNumberListToPgArrayText attNums =
  let
    commaDelimitedAttributeNumbers =
      mconcat $
        List.intersperse (LTB.singleton ',') (map attributeNumberTextBuilder attNums)
  in
    LT.toStrict . LTB.toLazyText $
      LTB.singleton '{' <> commaDelimitedAttributeNumbers <> LTB.singleton '}'
