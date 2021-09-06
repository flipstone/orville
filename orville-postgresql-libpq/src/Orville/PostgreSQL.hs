{- |
Module    : Orville.PostgreSQL.Raw
Copyright : Flipstone Technology Partners 2020-2021
License   : MIT
-}
module Orville.PostgreSQL
  ( EntityOperations.insertEntity,
    EntityOperations.insertEntities,
    EntityOperations.updateEntity,
    EntityOperations.deleteEntity,
    EntityOperations.findEntitiesBy,
    EntityOperations.findFirstEntityBy,
    EntityOperations.findEntity,
    Connection.createConnectionPool,
    TableDefinition.TableDefinition,
    TableDefinition.mkTableDefinition,
    TableDefinition.mkTableDefinitionWithoutKey,
    TableDefinition.setTableSchema,
    TableDefinition.tableName,
    TableDefinition.unqualifiedTableName,
    TableDefinition.unqualifiedTableNameString,
    TableDefinition.tableSchemaName,
    TableDefinition.tableSchemaNameString,
    TableDefinition.mkCreateTableExpr,
    TableDefinition.tablePrimaryKey,
    TableDefinition.tableMarshaller,
    TableDefinition.HasKey,
    TableDefinition.NoKey,
    PrimaryKey.PrimaryKey,
    PrimaryKey.primaryKey,
    PrimaryKey.compositePrimaryKey,
    SqlMarshaller.SqlMarshaller,
    SqlMarshaller.marshallField,
    SqlMarshaller.foldMarshallerFields,
    FieldDefinition.FieldDefinition,
    FieldDefinition.NotNull,
    FieldDefinition.Nullable,
    FieldDefinition.nullableField,
    FieldDefinition.asymmetricNullableField,
    FieldDefinition.convertField,
    FieldDefinition.coerceField,
    FieldDefinition.integerField,
    FieldDefinition.serialField,
    FieldDefinition.bigIntegerField,
    FieldDefinition.bigSerialField,
    FieldDefinition.doubleField,
    FieldDefinition.booleanField,
    FieldDefinition.unboundedTextField,
    FieldDefinition.boundedTextField,
    FieldDefinition.fixedTextField,
    FieldDefinition.textSearchVectorField,
    FieldDefinition.dateField,
    FieldDefinition.timestampField,
    FieldDefinition.timestampWithoutZoneField,
    FieldDefinition.fieldOfType,
    FieldDefinition.fieldColumnName,
    FieldDefinition.fieldName,
    FieldDefinition.fieldNameToString,
    FieldDefinition.fieldNameToByteString,
    FieldDefinition.fieldColumnDefinition,
    Orville.Orville,
    Orville.runOrville,
    MonadOrville.MonadOrville,
    MonadOrville.withConnection,
    Transaction.withTransaction,
    MonadOrville.MonadOrvilleControl (liftWithConnection),
    OrvilleState.HasOrvilleState (askOrvilleState, localOrvilleState),
    OrvilleState.OrvilleState,
    OrvilleState.newOrvilleState,
    OrvilleState.resetOrvilleState,
    SelectOptions.SelectOptions,
    SelectOptions.where_,
    SelectOptions.emptySelectOptions,
    SelectOptions.appendSelectOptions,
    SelectOptions.WhereCondition,
    SelectOptions.fieldEquals,
    SelectOptions.fieldNotEquals,
    SelectOptions.fieldGreaterThan,
    SelectOptions.fieldLessThan,
    SelectOptions.fieldGreaterThanOrEqualTo,
    SelectOptions.fieldLessThanOrEqualTo,
    SelectOptions.whereBooleanExpr,
    SelectOptions.whereAnd,
    SelectOptions.whereOr,
    SelectOptions.whereIn,
    SelectOptions.whereNotIn,
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
    SqlType.foreignRefType,
    SqlType.convertSqlType,
    SqlType.maybeConvertSqlType,
    Expr.QueryExpr,
    Execute.executeAndDecode,
    Execute.executeVoid,
  )
where

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.EntityOperations as EntityOperations
import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.Orville as Orville
import qualified Orville.PostgreSQL.Internal.OrvilleState as OrvilleState
import qualified Orville.PostgreSQL.Internal.PrimaryKey as PrimaryKey
import qualified Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlType as SqlType
import qualified Orville.PostgreSQL.Internal.TableDefinition as TableDefinition
import qualified Orville.PostgreSQL.Internal.Transaction as Transaction
