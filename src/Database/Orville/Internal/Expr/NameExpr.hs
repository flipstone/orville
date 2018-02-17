{-|
Module    : Database.Orville.Internal.Expr.NameExpr
Copyright : Fliptsone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE OverloadedStrings #-}
module Database.Orville.Internal.Expr.NameExpr where

import                Data.Monoid
import                Data.String

import                Database.Orville.Internal.Expr.Expr

type NameExpr = Expr NameForm

newtype NameForm = NameForm String
  deriving (Eq, Ord, IsString)

instance GenerateSql NameForm where
  generateSql (NameForm name) = "\"" <> rawSql name <> "\""

unescapedName :: NameForm -> String
unescapedName (NameForm s) = s
