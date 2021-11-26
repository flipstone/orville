{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Internal.Expr.Update.SetClauseList
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Update.SetClauseList
  ( SetClauseList,
    setClauseList,
  )
where

import Orville.PostgreSQL.Internal.Expr.Update.SetClause (SetClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SetClauseList
  = SetClauseList RawSql.RawSql
  deriving (RawSql.SqlExpression)

setClauseList :: [SetClause] -> SetClauseList
setClauseList =
  SetClauseList . RawSql.intercalate RawSql.comma
