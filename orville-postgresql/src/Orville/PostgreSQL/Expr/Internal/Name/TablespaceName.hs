{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.TablespaceName
  ( TablespaceName
  , tablespaceName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a PostgreSQL tablespace name. 'TablespaceName' values
constructed via the 'tablespaceName' function will be properly escaped as part
of the generated SQL. E.G.

> "some_tablespace_name"

'TablespaceName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype TablespaceName
  = TablespaceName Identifier
  deriving
    ( -- | @since 1.2.0.0
      IdentifierExpression
    , -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Construct a 'TablespaceName' from a 'String'.

@since 1.2.0.0
-}
tablespaceName :: String -> TablespaceName
tablespaceName = TablespaceName . identifier
