{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.AccessMethodName
  ( AccessMethodName
  , accessMethodName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the name of a PostgreSQL table access method, such as
@heap@. 'AccessMethodName' values constructed via the 'accessMethodName'
function will be properly escaped as part of the generated SQL. E.G.

> "heap"

'AccessMethodName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype AccessMethodName
  = AccessMethodName Identifier
  deriving
    ( -- | @since 1.2.0.0
      IdentifierExpression
    , -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Construct an 'AccessMethodName' from a 'String'.

@since 1.2.0.0
-}
accessMethodName :: String -> AccessMethodName
accessMethodName = AccessMethodName . identifier
