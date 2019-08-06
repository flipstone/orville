{-|
Module    : Database.Orville.Oracle.Internal.Expr.SelectExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.Oracle.Internal.Expr.SelectExpr where

import Data.Maybe

import Database.Orville.Oracle.Internal.MappendCompat ((<>))

import Database.Orville.Oracle.Internal.Expr.Expr
import Database.Orville.Oracle.Internal.Expr.NameExpr

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
