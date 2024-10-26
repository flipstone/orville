{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.IndexName
  ( IndexName
  , indexName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL index name. 'IndexName' values constructed via the
'indexName' function will be properly escaped as part of the generated SQL. E.G.

> "some_index_name"

'IndexName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype IndexName
  = IndexName Identifier
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    , -- | @since 1.0.0.0
      IdentifierExpression
    )

{- | Construct an 'IndexName' from a 'String' with proper escaping as part of the generated SQL.

@since 1.0.0.0
-}
indexName :: String -> IndexName
indexName = IndexName . identifier
