{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.PolicyName
  ( PolicyName
  , policyName
  ) where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL policy name. 'PolicyName' values constructed via
the 'policyName' function will be properly escaped as part of the generated
SQL. E.G.

> "some_policy_name"

'PolicyName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype PolicyName
  = PolicyName Identifier
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    , -- | @since 1.2.0.0
      IdentifierExpression
    )

{- | Construct a 'PolicyName' from a 'String' with proper escaping as part of the generated SQL.

@since 1.2.0.0
-}
policyName :: String -> PolicyName
policyName =
  PolicyName . identifier
