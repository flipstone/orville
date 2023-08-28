{-|
Module    : Database.Orville.PostgreSQL.Internal.Expr.SelectExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.PostgreSQL.Internal.Expr.SelectExpr where

import Data.Maybe

import Database.Orville.PostgreSQL.Internal.MappendCompat ((<>))

import Database.Orville.PostgreSQL.Internal.Expr.Expr
import Database.Orville.PostgreSQL.Internal.Expr.NameExpr

type SelectExpr = Expr SelectForm

data SelectForm = SelectForm
  { selectFormColumn :: NameForm
  , selectFormAlias :: Maybe NameForm
  }

selectColumn :: NameForm -> SelectForm
selectColumn name = SelectForm name Nothing

selectFormOutput :: SelectForm -> NameForm
selectFormOutput = fromMaybe <$> selectFormColumn <*> selectFormAlias

aliased :: SelectForm -> NameForm -> SelectForm
aliased sf name = sf {selectFormAlias = Just name}

instance QualifySql SelectForm where
  qualified form table =
    form {selectFormColumn = (selectFormColumn form) `qualified` table}

instance GenerateSql SelectForm where
  generateSql (SelectForm {..}) =
    generateSql selectFormColumn <> asOutput selectFormAlias

asOutput :: Maybe NameForm -> RawExpr
asOutput Nothing = mempty
asOutput (Just name) = " AS " <> generateSql name
