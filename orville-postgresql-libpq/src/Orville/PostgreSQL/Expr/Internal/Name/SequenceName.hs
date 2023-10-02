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
Type to represent a SQL sequence name. 'SequenceName' values constructed via
the 'sequenceName' function will be properly escaped as part of the generated
SQL. E.G.

> "some_sequence_name"

'SequenceName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

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
