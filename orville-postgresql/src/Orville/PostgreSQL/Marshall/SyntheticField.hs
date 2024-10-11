{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Marshall.SyntheticField
  ( SyntheticField
  , syntheticFieldExpression
  , syntheticFieldAlias
  , syntheticFieldValueFromSqlValue
  , syntheticField
  , nullableSyntheticField
  , prefixSyntheticField
  , orderBySyntheticField
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.AliasName (AliasName, aliasNameToByteString, byteStringToAliasName, stringToAliasName)
import qualified Orville.PostgreSQL.Marshall.SqlComparable as SqlComparable
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
  A 'SyntheticField' can be used to evaluate a SQL expression based on the
  columns of a table when records are selected from the database. Synthetic
  fields are inherently read-only.

@since 1.0.0.0
-}
data SyntheticField a = SyntheticField
  { i_syntheticFieldExpression :: Expr.ValueExpression
  , i_syntheticFieldAlias :: AliasName
  , i_syntheticFieldType :: SqlType.SqlType a
  }

{- |
  Marshalls a Haskell value to be in the field to its 'SqlValue.SqlValue'
  representation.

@since 1.1.0.0
-}
instance SqlComparable.SqlComparable (SyntheticField a) a where
  toComparableSqlValue field = SqlType.sqlTypeToSql (syntheticFieldType field)
  referenceValueExpression = syntheticFieldExpression

{- |
  Returns the SQL expression that should be used in select statements to
  calculate the synthetic field.

@since 1.0.0.0
-}
syntheticFieldExpression :: SyntheticField a -> Expr.ValueExpression
syntheticFieldExpression =
  i_syntheticFieldExpression

{- |
  Returns the alias that should be used in select statements to name the
  synthetic field.

@since 1.0.0.0
-}
syntheticFieldAlias :: SyntheticField a -> AliasName
syntheticFieldAlias =
  i_syntheticFieldAlias

{- |
  Decodes a calculated value selected from the database to its expected
  Haskell type. Returns a 'Left' with an error message if the decoding fails.

@since 1.0.0.0
-}
syntheticFieldValueFromSqlValue :: SyntheticField a -> SqlValue.SqlValue -> Either String a
syntheticFieldValueFromSqlValue =
  SqlType.sqlTypeFromSql . syntheticFieldType

{- |
  The 'SqlType.SqlType' for the 'SyntheticField' determines the PostgreSQL
  data type used to define the field as well as how to marshall Haskell values
  from the database.

@since 1.1.0.0
-}
syntheticFieldType :: SyntheticField a -> SqlType.SqlType a
syntheticFieldType = i_syntheticFieldType

{- |
  Constructs a 'SyntheticField' that will select a SQL expression using
  the given alias.

@since 1.0.0.0
-}
syntheticField ::
  -- | The SQL expression to be selected.
  Expr.ValueExpression ->
  -- | The alias to be used to name the calculation in SQL expressions.
  String ->
  -- | The 'SqlType.SqlType' to be used for comparisons
  SqlType.SqlType a ->
  SyntheticField a
syntheticField expression alias sqlType =
  SyntheticField
    { i_syntheticFieldExpression = expression
    , i_syntheticFieldAlias = stringToAliasName alias
    , i_syntheticFieldType = sqlType
    }

{- |
  Modifies a 'SyntheticField' to allow it to decode @NULL@ values.

@since 1.0.0.0
-}
nullableSyntheticField :: SyntheticField a -> SyntheticField (Maybe a)
nullableSyntheticField synthField =
  let
    nullableType :: SqlType.SqlType a -> SqlType.SqlType (Maybe a)
    nullableType sqlType =
      sqlType
        { SqlType.sqlTypeToSql = maybe SqlValue.sqlNull (SqlType.sqlTypeToSql sqlType)
        , SqlType.sqlTypeFromSql =
            \sqlValue ->
              if SqlValue.isSqlNull sqlValue
                then Right Nothing
                else fmap Just $ SqlType.sqlTypeFromSql sqlType sqlValue
        }
  in
    synthField
      { i_syntheticFieldType = nullableType (syntheticFieldType synthField)
      }

{- |
  Adds a prefix, followed by an underscore, to the alias used to name the
  synthetic field.

@since 1.0.0.0
-}
prefixSyntheticField ::
  String ->
  SyntheticField a ->
  SyntheticField a
prefixSyntheticField prefix synthField =
  synthField
    { i_syntheticFieldAlias = byteStringToAliasName (B8.pack prefix <> "_" <> aliasNameToByteString (syntheticFieldAlias synthField))
    }

{- |
  Orders a query by the alias for the given synthetic field.

@since 1.1.0.0
-}
orderBySyntheticField :: SyntheticField a -> Expr.OrderByDirection -> Expr.OrderByExpr
orderBySyntheticField =
  Expr.orderByColumnName . Expr.unqualified . Expr.fromIdentifier . Expr.identifierFromBytes . aliasNameToByteString . syntheticFieldAlias
