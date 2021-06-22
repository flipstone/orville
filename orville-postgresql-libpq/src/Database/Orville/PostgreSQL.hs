{- |
Module    : Database.Orville.PostgreSQL.Raw
Copyright : Flipstone Technology Partners 2020-2021
License   : MIT
-}
module Database.Orville.PostgreSQL
  ( RecordOperations.insertRecord,
    Connection.createConnectionPool,
    TableDefinition.TableDefinition,
    Orville.Orville,
    Orville.runOrville,
    MonadOrville.MonadOrville,
    MonadOrville.withConnection,
    MonadOrville.MonadOrvilleControl (liftWithConnection),
    MonadOrville.HasOrvilleState (askOrvilleState, localOrvilleState),
    MonadOrville.OrvilleState,
    SqlType.SqlType
      ( SqlType.SqlType,
        SqlType.sqlTypeExpr,
        SqlType.sqlTypeReferenceExpr,
        SqlType.sqlTypeNullable,
        SqlType.sqlTypeId,
        SqlType.sqlTypeSqlSize,
        SqlType.sqlTypeToSql,
        SqlType.sqlTypeFromSql
      ),

    -- * numeric types
    SqlType.integer,
    SqlType.serial,
    SqlType.bigInteger,
    SqlType.bigSerial,
    SqlType.double,

    -- * textual-ish types
    SqlType.boolean,
    SqlType.unboundedText,
    SqlType.fixedText,
    SqlType.boundedText,
    SqlType.textSearchVector,

    -- * date types
    SqlType.date,
    SqlType.timestamp,
    -- type conversions
    SqlType.nullableType,
    SqlType.foreignRefType,
    SqlType.convertSqlType,
    SqlType.maybeConvertSqlType,
    Expr.QueryExpr,
  )
where

import qualified Database.Orville.PostgreSQL.Connection as Connection
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Database.Orville.PostgreSQL.Internal.Orville as Orville
import qualified Database.Orville.PostgreSQL.Internal.RecordOperations as RecordOperations
import qualified Database.Orville.PostgreSQL.Internal.SqlType as SqlType
import qualified Database.Orville.PostgreSQL.Internal.TableDefinition as TableDefinition
