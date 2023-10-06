{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.FunctionName
  ( FunctionName
  , functionName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL function name. 'FunctionName' values constructed
via the 'functionName' function will be properly escaped as part of the
generated SQL. E.G.

> "some_function_name"

'FunctionName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype FunctionName
  = FunctionName Identifier
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    , -- | @since 0.10.0.0
      IdentifierExpression
    )

{- |
Construct a 'FunctionName' from a 'String' with proper escaping as part of the generated SQL.

@since 0.10.0.0
-}
functionName :: String -> FunctionName
functionName =
  FunctionName . identifier
