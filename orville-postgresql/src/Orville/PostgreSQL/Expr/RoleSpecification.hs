{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.RoleSpecification
  ( RoleSpecification
  , roleSpecification
  , public
  , currentRole
  , currentUser
  , sessionUser
  )
where

import Orville.PostgreSQL.Expr.Name (RoleName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a role as it is specified in statements such as
  @ALTER ... OWNER TO@ and @CREATE POLICY ... TO@: either a role by name, the
  @PUBLIC@ pseudo-role, or one of the keywords that PostgreSQL resolves to a
  role when the statement runs. E.G.

> CURRENT_USER

'RoleSpecification' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype RoleSpecification
  = RoleSpecification RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | Specifies the role with the given name.

@since 1.2.0.0
-}
roleSpecification :: RoleName -> RoleSpecification
roleSpecification =
  RoleSpecification . RawSql.toRawSql

{- | The @PUBLIC@ pseudo-role, which stands for all roles. PostgreSQL accepts
  it where a set of roles is named, such as the @TO@ clause of @CREATE POLICY@
  or @GRANT@, but not as the owner of a database object.

@since 1.2.0.0
-}
public :: RoleSpecification
public =
  RoleSpecification (RawSql.fromString "PUBLIC")

{- | The @CURRENT_ROLE@ keyword. Requires PostgreSQL 14 or later.

@since 1.2.0.0
-}
currentRole :: RoleSpecification
currentRole =
  RoleSpecification (RawSql.fromString "CURRENT_ROLE")

{- | The @CURRENT_USER@ keyword.

@since 1.2.0.0
-}
currentUser :: RoleSpecification
currentUser =
  RoleSpecification (RawSql.fromString "CURRENT_USER")

{- | The @SESSION_USER@ keyword.

@since 1.2.0.0
-}
sessionUser :: RoleSpecification
sessionUser =
  RoleSpecification (RawSql.fromString "SESSION_USER")
