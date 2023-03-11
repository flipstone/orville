{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Internal.Name.SavepointName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.SavepointName
  ( SavepointName,
    savepointName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SavepointName
  = SavepointName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

savepointName :: String -> SavepointName
savepointName = SavepointName . identifier
