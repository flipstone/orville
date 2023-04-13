{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2022
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.CursorName
  ( CursorName,
    cursorName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype CursorName
  = CursorName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

cursorName :: String -> CursorName
cursorName =
  CursorName . identifier
