{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.SavepointName
  ( SavepointName
  , savepointName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL savepoint name. 'SavepointName' values constructed via the
'savepointName' function will be properly escaped as part of the generated SQL.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) savepoint name by hand and
use it in a place that expected a 'SavepointName', that could be done as

 > RawSql.unsafeSqlExpression "my_savepoint_name"

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

savepointName :: String -> SavepointName
savepointName = SavepointName . identifier
