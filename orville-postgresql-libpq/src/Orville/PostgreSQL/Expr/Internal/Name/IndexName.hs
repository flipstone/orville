{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2021-2023
License   : MIT
Stability : Stable
-}
module Orville.PostgreSQL.Expr.Internal.Name.IndexName
  ( IndexName
  , indexName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL index name. 'IndexName' values constructed via the
'indexName' function will be properly escaped as part of the generated SQL.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) index name by hand and
use it in a place that expected an 'IndexName', that could be done as

 > RawSql.unsafeSqlExpression "my_index_name"

@since 0.10.0.0
-}
newtype IndexName
  = IndexName Identifier
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    , -- | @since 0.10.0.0
      IdentifierExpression
    )

{- |
Construct a 'IndexName' from a 'String' with proper escaping as part of the generated SQL.

@since 0.10.0.0
-}
indexName :: String -> IndexName
indexName = IndexName . identifier
