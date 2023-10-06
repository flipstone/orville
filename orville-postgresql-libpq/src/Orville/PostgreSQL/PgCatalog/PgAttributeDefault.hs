{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgAttributeDefault
  ( PgAttributeDefault (..)
  , pgAttributeDefaultTable
  , attributeDefaultRelationOidField
  )
where

import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField, oidTypeField)
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeNumber, attributeNumberTypeField)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
  The Haskell representation of data read from the @pg_catalog.pg_attrdef@
  table.

@since 0.10.0.0
-}
data PgAttributeDefault = PgAttributeDefault
  { pgAttributeDefaultOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the default value
  , pgAttributeDefaultRelationOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the relation that this
  -- attribute belongs to. References @pg_class.oid@
  , pgAttributeDefaultAttributeNumber :: AttributeNumber
  -- ^ The PostgreSQL attribute number for the column that this
  -- default belongs to. References @pg_attribute.attnum@.
  , pgAttributeDefaultExpression :: T.Text
  -- ^ The PostgreSQL default value expression, as decompiled from the
  -- @adbin@ column using the PostgreSQL @pg_get_expr@ function.
  }

{- |
  An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_attrdef@ table

@since 0.10.0.0
-}
pgAttributeDefaultTable :: Orville.TableDefinition Orville.NoKey PgAttributeDefault PgAttributeDefault
pgAttributeDefaultTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey
      "pg_attrdef"
      pgAttributeDefaultMarshaller

pgAttributeDefaultMarshaller :: Orville.SqlMarshaller PgAttributeDefault PgAttributeDefault
pgAttributeDefaultMarshaller =
  PgAttributeDefault
    <$> Orville.marshallField pgAttributeDefaultOid oidField
    <*> Orville.marshallField pgAttributeDefaultRelationOid attributeDefaultRelationOidField
    <*> Orville.marshallField pgAttributeDefaultAttributeNumber attributeDefaultAttributeNumberField
    <*> Orville.marshallSyntheticField attributeDefaultExpressionField

{- |
  The @adrelid@ column of the @pg_catalog.pg_attrdef@ table

@since 0.10.0.0
-}
attributeDefaultRelationOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
attributeDefaultRelationOidField =
  oidTypeField "adrelid"

{- |
  The @adnum@ column of the @pg_catalog.pg_attrdef@ table

@since 0.10.0.0
-}
attributeDefaultAttributeNumberField :: Orville.FieldDefinition Orville.NotNull AttributeNumber
attributeDefaultAttributeNumberField =
  attributeNumberTypeField "adnum"

{- |
  A syntheticField for selecting the default expression by decompling the
  @adbin@ column of the @pg_catalog.pg_attrdef@ table. The @pg_node_tree@ found
  in the column is decompiled by selecting the expression
  @pg_get_expr(adbin,adrelid)@.

@since 0.10.0.0
-}
attributeDefaultExpressionField :: Orville.SyntheticField T.Text
attributeDefaultExpressionField =
  Orville.syntheticField
    (RawSql.unsafeSqlExpression "pg_get_expr(adbin,adrelid)")
    "expression"
    SqlValue.toText
