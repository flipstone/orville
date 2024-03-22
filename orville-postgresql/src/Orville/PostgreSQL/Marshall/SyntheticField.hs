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
import Orville.PostgreSQL.Marshall.FieldDefinition (FieldName, byteStringToFieldName, fieldNameToByteString, fieldNameToColumnName, stringToFieldName)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
  A 'SyntheticField' can be used to evaluate a SQL expression based on the
  columns of a table when records are selected from the database. Synthetic
  fields are inherently read-only.

@since 1.0.0.0
-}
data SyntheticField a = SyntheticField
  { _syntheticFieldExpression :: Expr.ValueExpression
  , _syntheticFieldAlias :: FieldName
  , _syntheticFieldValueFromSqlValue :: SqlValue.SqlValue -> Either String a
  }

{- |
  Returns the SQL expression that should be used in select statements to
  calculate the synthetic field.

@since 1.0.0.0
-}
syntheticFieldExpression :: SyntheticField a -> Expr.ValueExpression
syntheticFieldExpression =
  _syntheticFieldExpression

{- |
  Returns the alias that should be used in select statements to name the
  synthetic field.

@since 1.0.0.0
-}
syntheticFieldAlias :: SyntheticField a -> FieldName
syntheticFieldAlias =
  _syntheticFieldAlias

{- |
  Decodes a calculated value selected from the database to its expected
  Haskell type. Returns a 'Left' with an error message if the decoding fails.

@since 1.0.0.0
-}
syntheticFieldValueFromSqlValue :: SyntheticField a -> SqlValue.SqlValue -> Either String a
syntheticFieldValueFromSqlValue =
  _syntheticFieldValueFromSqlValue

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
  -- | A function to decode the expression result from a 'SqlValue.SqlValue'.
  (SqlValue.SqlValue -> Either String a) ->
  SyntheticField a
syntheticField expression alias fromSqlValue =
  SyntheticField
    { _syntheticFieldExpression = expression
    , _syntheticFieldAlias = stringToFieldName alias
    , _syntheticFieldValueFromSqlValue = fromSqlValue
    }

{- |
  Modifies a 'SyntheticField' to allow it to decode @NULL@ values.

@since 1.0.0.0
-}
nullableSyntheticField :: SyntheticField a -> SyntheticField (Maybe a)
nullableSyntheticField synthField =
  synthField
    { _syntheticFieldValueFromSqlValue = \sqlValue ->
        if SqlValue.isSqlNull sqlValue
          then Right Nothing
          else Just <$> syntheticFieldValueFromSqlValue synthField sqlValue
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
    { _syntheticFieldAlias = byteStringToFieldName (B8.pack prefix <> "_" <> fieldNameToByteString (syntheticFieldAlias synthField))
    }

{- |
  Orders a query by the alias for the given synthetic field.

@since 1.1.0.0
-}
orderBySyntheticField :: SyntheticField a -> Expr.OrderByDirection -> Expr.OrderByExpr
orderBySyntheticField =
  Expr.orderByColumnName . fieldNameToColumnName . syntheticFieldAlias
