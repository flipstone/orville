{-|
Module    : Database.Orville.PostgreSQL.Expr
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr
  ( QueryExpr( QueryExpr
              , selectList
              , tableExpression
              )
  , TableExpr ( TableExpr
              , fromClause
              )
  , InsertExpr ( InsertExpr
               , target
               , rowValues
               )
  , queryExprToSql
  , insertExprToSql
  ) where

import qualified Data.ByteString.Char8 as B8

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
data QueryExpr = QueryExpr
  { selectList :: [B8.ByteString]
  , tableExpression :: TableExpr
  }

data TableExpr = TableExpr
  { fromClause :: B8.ByteString
-- where, groupby, having, etc
  }

queryExprToSql :: QueryExpr -> B8.ByteString
queryExprToSql queryExpr =
  B8.concat
    [ B8.pack "SELECT "
    , B8.intercalate (B8.pack ",") (selectList queryExpr)
    , B8.pack " FROM "
    , fromClause (tableExpression queryExpr)
    ]

data InsertExpr =
  InsertExpr
    { target :: B8.ByteString
    , rowValues :: [B8.ByteString]
    }

insertExprToSql :: InsertExpr -> B8.ByteString
insertExprToSql insertExpr =
  B8.concat
    [ B8.pack "INSERT INTO "
    , target insertExpr
    , B8.pack " VALUES ("
    , B8.intercalate (B8.pack ",") (rowValues insertExpr)
    , B8.pack ")"
    ]
