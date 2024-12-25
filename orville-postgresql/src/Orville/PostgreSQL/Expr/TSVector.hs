{- |
Copyright : Flipstone Technology Partners 2024-2025
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.TSVector (
      toTSVector 
    , tsMatch
    , toTSQuery
    , Regconfig(.. )   
) where

import Orville.PostgreSQL.Expr.Name (FunctionName, functionName)
import Orville.PostgreSQL.Expr.BinaryOperator (BinaryOperator, binaryOperator, binaryOpExpression)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, functionCall)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import Data.Char (toLower)

{- | Represents a text search configuration. 

 @since 1.1.0.0
-}
data Regconfig = Simple
  | Arabic
  | Armenian
  | Basque
  | Catalan
  | Danish
  | Dutch
  | English
  | Finnish
  | French
  | German
  | Greek
  | Hindi
  | Hungarian
  | Indonesian
  | Irish
  | Italian
  | Lithuanian
  | Nepali
  | Norwegian
  | Portuguese
  | Romanian
  | Russian
  | Serbian
  | Spanish
  | Swedish
  | Tamil
  | Turkish
  | Yiddish
    deriving (Eq, Show)

tsMatchOp :: BinaryOperator
tsMatchOp = binaryOperator "@@"

{- | Builds a ts_match boolean expression from two value expressions. Alias of @@ operator.

@since 1.1.0.0
-}
tsMatch :: ValueExpression -> ValueExpression -> BooleanExpr
tsMatch tsVector tsQuery = binaryOpExpression tsMatchOp tsVector tsQuery

toTSVectorFunction :: FunctionName
toTSVectorFunction = functionName "to_tsvector"

toTSQueryFunction :: FunctionName
toTSQueryFunction = functionName "to_tsquery"

regconfigToValueExpression :: Regconfig -> ValueExpression
regconfigToValueExpression regconfig = RawSql.unsafeFromRawSql $ (RawSql.fromString "\'" <>
    (RawSql.fromString $ map toLower (show regconfig)) <>
    RawSql.fromString "\'")


{- | Builds a tsvector from a value expression. If a regconfig is provided, it will be used to build the tsvector. 

@since 1.1.0.0
-}
toTSVector :: ValueExpression -> Maybe Regconfig -> ValueExpression
toTSVector val mRegconfig = 
    let valueExpressions = case mRegconfig of
                                Nothing -> mempty
                                Just regconfig -> [regconfigToValueExpression regconfig] 
        in functionCall toTSVectorFunction (valueExpressions <> [val])


{- | Builds a tsquery from a value expression. If a regconfig is provided, it will be used to build the tsvector. 

@since 1.1.0.0
-}
toTSQuery :: ValueExpression -> Maybe Regconfig -> ValueExpression
toTSQuery val mRegconfig = 
    let valueExpressions = case mRegconfig of
                                Nothing -> mempty
                                Just regconfig -> [regconfigToValueExpression regconfig] 
        in functionCall toTSQueryFunction (valueExpressions <> [val])