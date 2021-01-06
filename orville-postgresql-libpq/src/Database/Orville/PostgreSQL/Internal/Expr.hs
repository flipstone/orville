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

import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEnc
import qualified Data.ByteString.Char8 as B8

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
data QueryExpr = QueryExpr
  { selectList :: [T.Text]
  , tableExpression :: TableExpr
  }

data TableExpr = TableExpr
  { fromClause :: T.Text
-- where, groupby, having, etc
  }

queryExprToSql :: QueryExpr -> B8.ByteString
queryExprToSql queryExpr =
  TextEnc.encodeUtf8 $
    T.concat
      [ T.pack "SELECT "
      , T.intercalate (T.pack ",") (selectList queryExpr)
      , T.pack " FROM "
      , fromClause (tableExpression queryExpr)
      ]

data InsertExpr =
  InsertExpr
    { target :: T.Text
    , rowValues :: [T.Text]
    }

insertExprToSql :: InsertExpr -> B8.ByteString
insertExprToSql insertExpr =
  TextEnc.encodeUtf8 $
    T.concat
      [ T.pack "INSERT INTO "
      , target insertExpr
      , T.pack " VALUES ("
      , T.intercalate (T.pack ",") (rowValues insertExpr)
      , T.pack ")"
      ]
