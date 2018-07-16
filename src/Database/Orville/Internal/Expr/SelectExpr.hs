{-|
Module    : Database.Orville.Internal.Expr.SelectExpr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.Internal.Expr.SelectExpr where

import Data.Maybe
import Data.Monoid

import Database.Orville.Internal.Expr.Expr
import Database.Orville.Internal.Expr.NameExpr

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

instance GenerateSql SelectForm where
  generateSql (SelectForm {..}) =
    generateSql selectFormColumn <>
    asOutput selectFormAlias

asOutput :: Maybe NameForm -> RawExpr
asOutput Nothing = mempty
asOutput (Just name) = " AS " <> generateSql name
