{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.SavepointName
  ( SavepointName
  , savepointName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL savepoint name. 'SavepointName' values constructed via
the 'savepointName' function will be properly escaped as part of the generated
SQL. E.G.

> "some_savepoint_name"

'SavepointName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype SavepointName
  = SavepointName Identifier
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    , -- | @since 0.10.0.0
      IdentifierExpression
    )

{- |
Construct a 'SavepointName' from a 'String' with proper escaping as part of the generated SQL.

@since 0.10.0.0
-}
savepointName :: String -> SavepointName
savepointName = SavepointName . identifier
