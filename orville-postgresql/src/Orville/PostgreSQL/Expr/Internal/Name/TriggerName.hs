{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.TriggerName
  ( TriggerName
  , triggerName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL trigger name. 'TriggerName' values constructed via the
'triggerName' function will be properly escaped as part of the generated SQL.
E.G.

> "some_trigger_name"

'TriggerName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype TriggerName
  = TriggerName Identifier
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    , -- | @since 1.1.0.0
      IdentifierExpression
    )

{- |
Construct a 'TriggerName' from a 'String' with proper escaping as part of the generated SQL.

@since 1.1.0.0
-}
triggerName :: String -> TriggerName
triggerName =
  TriggerName . identifier
