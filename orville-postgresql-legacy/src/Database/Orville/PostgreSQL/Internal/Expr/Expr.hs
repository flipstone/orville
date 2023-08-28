{-|
Module    : Database.Orville.PostgreSQL.Internal.Expr.Expr
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-#LANGUAGE CPP#-}

module Database.Orville.PostgreSQL.Internal.Expr.Expr where

import Data.String

data RawExpr
  = RawExprString String
  | RawExprAppend RawExpr
                  RawExpr
  | RawExprConcat [RawExpr]

rawSql :: String -> RawExpr
rawSql = RawExprString

rawExprToSql :: RawExpr -> String
rawExprToSql = go ""
  where
    go rest (RawExprString s) = s ++ rest
    go rest (RawExprAppend r1 r2) = go (go rest r2) r1
    go rest (RawExprConcat exprs) = foldr (flip go) rest exprs

#if MIN_VERSION_base(4,11,0)
instance Semigroup RawExpr where
  (<>) = RawExprAppend
#endif

instance Monoid RawExpr where
  mempty = RawExprString ""
  mappend = RawExprAppend
  mconcat = RawExprConcat

instance IsString RawExpr where
  fromString = rawSql

newtype Expr a =
  Expr (Either RawExpr a)

class QualifySql form where
  qualified :: form -> String -> form

instance QualifySql a => QualifySql (Expr a) where
  qualified (Expr (Right a)) table = Expr . Right $ qualified a table
  qualified (Expr (Left raw)) _ = Expr . Left $ raw

class GenerateSql expr where
  generateSql :: expr -> RawExpr

instance GenerateSql RawExpr where
  generateSql = id

instance GenerateSql a => GenerateSql (Expr a) where
  generateSql (Expr (Right a)) = generateSql a
  generateSql (Expr (Left raw)) = raw

rawSqlExpr :: String -> Expr a
rawSqlExpr = Expr . Left . rawSql

expr :: a -> Expr a
expr = Expr . Right
