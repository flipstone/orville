{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2022-2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.SequenceName
  ( SequenceName
  , sequenceName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL sequence name. 'SequenceName' values constructed via the
'sequenceName' function will be properly escaped as part of the generated SQL.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) sequence name by hand and
use it in a place that expected a 'SequenceName', that could be done as

 > RawSql.unsafeSqlExpression "my_sequence_name"

@since 0.10.0.0
-}
newtype SequenceName
  = SequenceName Identifier
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    , -- | @since 0.10.0.0
      IdentifierExpression
    )

{- |
Construct a 'SequenceName' from a 'String' with proper escaping as part of the generated SQL.

@since 0.10.0.0
-}
sequenceName :: String -> SequenceName
sequenceName =
  SequenceName . identifier
