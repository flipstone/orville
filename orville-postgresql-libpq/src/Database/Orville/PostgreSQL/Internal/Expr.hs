{-|
Module    : Database.Orville.PostgreSQL.Expr
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr
  ( QueryExpr
  , queryExpr
  , queryExprToSql
  , SelectList
  , selectStar
  , selectColumns
  , TableExpr
  , tableExpr
  , TableName
  , tableNameToSql
  , rawTableName
  , ColumnName
  , rawColumnName
  , columnNameToSql
  , WhereClause
  , whereClause
  , BooleanExpr
  , orExpr
  , andExpr
  , parenthesized
  , comparison
  , columnEquals
  , columnGreaterThan
  , columnLessThan
  , columnGreaterThanOrEqualTo
  , columnLessThanOrEqualTo
  , RowValuePredicand
  , columnReference
  , comparisonValue
  , ComparisonOperator
  , equalsOp
  , notEqualsOp
  , greaterThanOp
  , lessThanOp
  , greaterThanOrEqualsOp
  , lessThanOrEqualsOp
  , InsertExpr
  , insertExpr
  , insertExprToSql
  , InsertSource
  , insertSqlValues
  , insertRowValues
  , RowValues
  , rowValues
  , DataType
  , dataTypeToSql
  , timestampWithZone
  , date
  , tsvector
  , varchar
  , char
  , text
  , boolean
  , doublePrecision
  , bigSerial
  , bigInt
  , serial
  , int
  , FieldDefinition
  , fieldDefinition
  , fieldDefinitionToSql
  , OrderByClause
  , orderByClauseToSql
  , orderByClause
  , appendOrderBy
  , ascendingOrder
  , descendingOrder
  , orderByExpr
  ) where

import           Database.Orville.PostgreSQL.Internal.RawSql (RawSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import           Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
newtype QueryExpr =
  QueryExpr RawSql

queryExpr :: SelectList -> TableExpr -> QueryExpr
queryExpr selectList table =
  QueryExpr $
    mconcat
      [ RawSql.fromString "SELECT "
      , selectListToSql selectList
      , RawSql.fromString " FROM "
      , tableExprToSql table
      ]

queryExprToSql :: QueryExpr -> RawSql
queryExprToSql (QueryExpr sql) = sql

newtype SelectList =
  SelectList RawSql

selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

selectColumns :: [ColumnName] -> SelectList
selectColumns columnNames =
  SelectList $
    RawSql.intercalate
      (RawSql.fromString ",")
      (map columnNameToSql columnNames)

selectListToSql :: SelectList -> RawSql
selectListToSql (SelectList sql) =
  sql

newtype TableExpr =
  TableExpr RawSql

tableExprToSql :: TableExpr -> RawSql
tableExprToSql (TableExpr sql) = sql

tableExpr :: TableName -> Maybe WhereClause -> Maybe OrderByClause -> TableExpr
tableExpr tableName mbWhereClause maybeOrderByClause =
  TableExpr $
    tableNameToSql tableName
    <> RawSql.fromString " "
    <> maybe mempty whereClauseToSql mbWhereClause
    <> maybe mempty orderByClauseToSql maybeOrderByClause

newtype TableName =
  TableName RawSql

tableNameToSql :: TableName -> RawSql
tableNameToSql (TableName sql) = sql

rawTableName :: String -> TableName
rawTableName =
  TableName . RawSql.fromString

newtype WhereClause =
  WhereClause RawSql

whereClauseToSql :: WhereClause -> RawSql
whereClauseToSql (WhereClause sql) = sql

whereClause :: BooleanExpr -> WhereClause
whereClause booleanExpr =
  WhereClause $
    RawSql.fromString "WHERE " <> booleanExprToSql booleanExpr

newtype BooleanExpr =
  BooleanExpr RawSql

booleanExprToSql :: BooleanExpr -> RawSql
booleanExprToSql (BooleanExpr sql) = sql

orExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
orExpr left right =
  BooleanExpr $
    booleanExprToSql left
    <> RawSql.fromString " OR "
    <> booleanExprToSql right

andExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
andExpr left right =
  BooleanExpr $
    booleanExprToSql left
    <> RawSql.fromString " AND "
    <> booleanExprToSql right

parenthesized :: BooleanExpr -> BooleanExpr
parenthesized expr =
  BooleanExpr $
    RawSql.fromString "(" <> booleanExprToSql expr <> RawSql.fromString ")"

comparison :: RowValuePredicand
           -> ComparisonOperator
           -> RowValuePredicand
           -> BooleanExpr
comparison left op right =
  BooleanExpr $
    rowValuePredicandToSql left
    <> RawSql.fromString " "
    <> comparisonOperatiorToSql op
    <> RawSql.fromString " "
    <> rowValuePredicandToSql right

newtype OrderByClause =
  OrderByClause RawSql

orderByClauseToSql :: OrderByClause -> RawSql
orderByClauseToSql (OrderByClause sql) = sql

orderByClause :: OrderByExpr -> OrderByClause
orderByClause expr = OrderByClause (RawSql.fromString "ORDER BY " <> orderByExprToSql expr)

newtype OrderByExpr =
  OrderByExpr RawSql

orderByExprToSql :: OrderByExpr -> RawSql
orderByExprToSql (OrderByExpr sql) = sql

newtype OrderByDirection =
  OrderByDirection RawSql

ascendingOrder :: OrderByDirection
ascendingOrder = OrderByDirection $ RawSql.fromString "ASC"

descendingOrder :: OrderByDirection
descendingOrder = OrderByDirection $ RawSql.fromString "DESC"

orderByExpr :: RawSql -> OrderByDirection -> OrderByExpr
orderByExpr sql (OrderByDirection orderSql) =
  OrderByExpr $ sql <> RawSql.fromString " " <> orderSql

appendOrderBy :: OrderByExpr -> OrderByExpr -> OrderByExpr
appendOrderBy (OrderByExpr a) (OrderByExpr b) = OrderByExpr (a <> RawSql.fromString ", " <> b)

newtype ComparisonOperator =
  ComparisonOperator RawSql

comparisonOperatiorToSql :: ComparisonOperator -> RawSql
comparisonOperatiorToSql (ComparisonOperator sql) = sql

equalsOp :: ComparisonOperator
equalsOp =
  ComparisonOperator (RawSql.fromString "=")

notEqualsOp :: ComparisonOperator
notEqualsOp =
  ComparisonOperator (RawSql.fromString "<>")

greaterThanOp :: ComparisonOperator
greaterThanOp =
  ComparisonOperator (RawSql.fromString ">")

lessThanOp :: ComparisonOperator
lessThanOp =
  ComparisonOperator (RawSql.fromString "<")

greaterThanOrEqualsOp :: ComparisonOperator
greaterThanOrEqualsOp =
  ComparisonOperator (RawSql.fromString ">=")

lessThanOrEqualsOp :: ComparisonOperator
lessThanOrEqualsOp =
  ComparisonOperator (RawSql.fromString "<=")

columnEquals :: ColumnName -> SqlValue -> BooleanExpr
columnEquals name value =
  comparison (columnReference name) equalsOp (comparisonValue value)

columnGreaterThan :: ColumnName -> SqlValue -> BooleanExpr
columnGreaterThan name value =
  comparison (columnReference name) greaterThanOp (comparisonValue value)

columnLessThan :: ColumnName -> SqlValue -> BooleanExpr
columnLessThan name value =
  comparison (columnReference name) lessThanOp (comparisonValue value)

columnGreaterThanOrEqualTo :: ColumnName -> SqlValue -> BooleanExpr
columnGreaterThanOrEqualTo name value =
  comparison (columnReference name) greaterThanOrEqualsOp (comparisonValue value)

columnLessThanOrEqualTo :: ColumnName -> SqlValue -> BooleanExpr
columnLessThanOrEqualTo name value =
  comparison (columnReference name) lessThanOrEqualsOp (comparisonValue value)

newtype RowValuePredicand =
  RowValuePredicand RawSql

rowValuePredicandToSql :: RowValuePredicand -> RawSql
rowValuePredicandToSql (RowValuePredicand sql) = sql

columnReference :: ColumnName -> RowValuePredicand
columnReference =
  RowValuePredicand . columnNameToSql

comparisonValue :: SqlValue -> RowValuePredicand
comparisonValue =
  RowValuePredicand . RawSql.parameter

newtype ColumnName =
  ColumnName RawSql

columnNameToSql :: ColumnName -> RawSql
columnNameToSql (ColumnName sql) = sql

rawColumnName :: String -> ColumnName
rawColumnName =
  ColumnName . RawSql.fromString

newtype InsertExpr =
  InsertExpr RawSql

insertExpr :: TableName -> InsertSource -> InsertExpr
insertExpr target source =
  InsertExpr $
    mconcat
      [ RawSql.fromString "INSERT INTO "
      , tableNameToSql target
      , RawSql.fromString " "
      , insertSourceToSql source
      ]

newtype InsertSource =
  InsertSource RawSql

insertSourceToSql :: InsertSource -> RawSql
insertSourceToSql (InsertSource sql) = sql

insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
    <> RawSql.intercalate (RawSql.fromString ",") (map rowValuesToSql rows)

insertSqlValues :: [[SqlValue]] -> InsertSource
insertSqlValues rows =
  insertRowValues (map rowValues rows)

newtype RowValues =
  RowValues RawSql

rowValuesToSql :: RowValues -> RawSql
rowValuesToSql (RowValues sql) = sql

rowValues :: [SqlValue] -> RowValues
rowValues values =
  RowValues $
    mconcat
      [ RawSql.fromString "("
      , RawSql.intercalate (RawSql.fromString ",") (map RawSql.parameter values)
      , RawSql.fromString ")"
      ]

insertExprToSql :: InsertExpr -> RawSql
insertExprToSql (InsertExpr sql) = sql

newtype DataType =
  DataType RawSql

dataTypeToSql :: DataType -> RawSql
dataTypeToSql (DataType sql) = sql

timestampWithZone :: DataType
timestampWithZone =
  DataType (RawSql.fromString "TIMESTAMP with time zone")

date :: DataType
date =
  DataType (RawSql.fromString "DATE")

tsvector :: DataType
tsvector =
  DataType (RawSql.fromString "TSVECTOR")

varchar :: Int -> DataType
varchar len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo VARCHAR($1))
  DataType $
    RawSql.fromString "VARCHAR("
    <> RawSql.fromString (show len)
    <> RawSql.fromString ")"

char :: Int -> DataType
char len =
  -- postgresql won't let us pass the field length as a parameter.
  -- when we try we get an error like such error:
  --  ERROR:  syntax error at or near "$1" at character 48
  --  STATEMENT:  CREATE TABLE field_definition_test(foo CHAR($1))
  DataType $
    RawSql.fromString "CHAR("
    <> RawSql.fromString (show len)
    <> RawSql.fromString ")"

text :: DataType
text =
  DataType (RawSql.fromString "TEXT")

boolean :: DataType
boolean =
  DataType (RawSql.fromString "BOOLEAN")

doublePrecision :: DataType
doublePrecision =
  DataType (RawSql.fromString "DOUBLE PRECISION")

bigSerial :: DataType
bigSerial =
  DataType (RawSql.fromString "BIGSERIAL")

bigInt :: DataType
bigInt =
  DataType (RawSql.fromString "BIGINT")

serial :: DataType
serial =
  DataType (RawSql.fromString "SERIAL")

int :: DataType
int =
  DataType (RawSql.fromString "INT")

newtype FieldDefinition =
  FieldDefinition RawSql

fieldDefinitionToSql :: FieldDefinition -> RawSql
fieldDefinitionToSql (FieldDefinition sql) = sql

fieldDefinition :: ColumnName -> DataType -> FieldDefinition
fieldDefinition columnName dataType =
  FieldDefinition $
    columnNameToSql columnName
    <> RawSql.fromString " "
    <> dataTypeToSql dataType
