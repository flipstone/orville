{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Comment
  ( Comment
  , commentText
  , CommentExpr
  , commentTableExpr
  , commentColumnExpr
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc

import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified, SchemaName, TableName, qualifyColumn)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a PostgreSQL comment string literal.

@since 1.1.0.0
-}
newtype Comment = Comment RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
Construct a 'Comment' from a 'T.Text' value. The value will be escaped and quoted.

@since 1.1.0.0
-}
commentText :: T.Text -> Comment
commentText = Comment . RawSql.stringLiteral . TEnc.encodeUtf8

{- |
Type to represent a PostgreSQL @COMMENT@ statement.

@since 1.1.0.0
-}
newtype CommentExpr = CommentExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
Construct a 'CommentExpr' for a @COMMENT ON TABLE@ statement.

@since 1.1.0.0
-}
commentTableExpr :: Qualified TableName -> Maybe Comment -> CommentExpr
commentTableExpr tableName mbComment =
  CommentExpr $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "COMMENT ON TABLE"
      , RawSql.toRawSql tableName
      , RawSql.fromString "IS"
      , maybe RawSql.nullLiteral RawSql.toRawSql mbComment
      ]

{- |
Construct a 'CommentExpr' for a @COMMENT ON COLUMN@ statement.

@since 1.1.0.0
-}
commentColumnExpr :: Maybe SchemaName -> TableName -> ColumnName -> Maybe Comment -> CommentExpr
commentColumnExpr mbSchema tableName colName mbComment =
  CommentExpr $
    RawSql.intercalate
      RawSql.space
      [ RawSql.fromString "COMMENT ON COLUMN"
      , RawSql.toRawSql $ qualifyColumn mbSchema tableName colName
      , RawSql.fromString "IS"
      , maybe RawSql.nullLiteral RawSql.toRawSql mbComment
      ]
