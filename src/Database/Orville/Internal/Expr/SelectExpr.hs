{-|
Module    : Database.Orville.Internal.Expr.SelectExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.Internal.Expr.SelectExpr
( SelectExpr
, SelectForm
, selectColumn
, selectFormOutput
, aliased
) where

import Data.Monoid

import Database.Orville.Internal.Expr.Expr
import Database.Orville.Internal.Expr.NameExpr

type SelectExpr = Expr SelectForm

data SelectForm = SelectForm
  { selectFormColumn :: NameForm
  , selectFormAlias :: Maybe NameForm
  }

selectColumn :: NameForm -> SelectForm
selectColumn form = SelectForm form Nothing

selectFormOutput :: SelectForm -> NameForm
selectFormOutput form = case selectFormAlias form of
  Just alias -> alias
  Nothing -> case selectFormColumn form of
    (NameForm Nothing _) -> selectFormColumn form
    (NameForm (Just table) name) -> (NameForm Nothing (table ++ "." ++ name))

aliased :: SelectForm -> NameForm -> SelectForm
aliased sf name = sf {selectFormAlias = Just name}

instance QualifySql SelectForm where
  qualified form table =
    form { selectFormColumn = (selectFormColumn form) `qualified` table }

instance GenerateSql SelectForm where
  generateSql form =
    generateSql (selectFormColumn form) <>
    asOutput (selectFormOutput form)

asOutput :: NameForm -> RawExpr
asOutput name = " AS " <> generateSql name
