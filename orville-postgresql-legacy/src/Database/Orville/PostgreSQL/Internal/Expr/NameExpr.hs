{-|
Module    : Database.Orville.PostgreSQL.Internal.Expr.NameExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Orville.PostgreSQL.Internal.Expr.NameExpr where

import Data.String

import Database.Orville.PostgreSQL.Internal.MappendCompat ((<>))

import Database.Orville.PostgreSQL.Internal.Expr.Expr
import Database.Orville.PostgreSQL.Internal.QueryKey

type NameExpr = Expr NameForm

data NameForm = NameForm
  { nameFormTable :: Maybe String
  , nameFormName :: String
  } deriving (Eq, Ord)

instance IsString NameForm where
  fromString str = NameForm {nameFormTable = Nothing, nameFormName = str}

instance QualifySql NameForm where
  qualified form table = form {nameFormTable = Just table}

instance QueryKeyable NameForm where
  queryKey = QKField . unescapedName

instance GenerateSql NameForm where
  generateSql (NameForm Nothing name) = "\"" <> rawSql name <> "\""
  generateSql (NameForm (Just table) name) =
    "\"" <> rawSql table <> "\".\"" <> rawSql name <> "\""

unescapedName :: NameForm -> String
unescapedName (NameForm Nothing name) = name
unescapedName (NameForm (Just table) name) = table <> "." <> name
