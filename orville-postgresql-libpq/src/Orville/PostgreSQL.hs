{- |
Module    : Orville.PostgreSQL.Raw
Copyright : Flipstone Technology Partners 2020-2021
License   : MIT
-}
module Orville.PostgreSQL
  ( EntityOperations.insertEntity,
    EntityOperations.insertAndReturnEntity,
    EntityOperations.insertEntities,
    EntityOperations.insertAndReturnEntities,
    EntityOperations.updateEntity,
    EntityOperations.updateAndReturnEntity,
    EntityOperations.deleteEntity,
    EntityOperations.deleteAndReturnEntity,
    EntityOperations.findEntitiesBy,
    EntityOperations.findFirstEntityBy,
    EntityOperations.findEntity,
    Connection.createConnectionPool,
    TableDefinition.TableDefinition,
    TableDefinition.mkTableDefinition,
    TableDefinition.mkTableDefinitionWithoutKey,
    TableDefinition.setTableSchema,
    TableDefinition.dropColumns,
    TableDefinition.columnsToDrop,
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
    SqlMarshaller.marshallReadOnly,
    SqlMarshaller.marshallReadOnlyField,
    SqlMarshaller.foldMarshallerFields,
    SqlMarshaller.partialMap,
    FieldDefinition.FieldDefinition,
    FieldDefinition.NotNull,
    FieldDefinition.Nullable,
    FieldDefinition.nullableField,
    FieldDefinition.asymmetricNullableField,
    FieldDefinition.convertField,
    FieldDefinition.coerceField,
    FieldDefinition.integerField,
    FieldDefinition.serialField,
    FieldDefinition.smallIntegerField,
    FieldDefinition.bigIntegerField,
    FieldDefinition.bigSerialField,
    FieldDefinition.doubleField,
    FieldDefinition.booleanField,
    FieldDefinition.unboundedTextField,
    FieldDefinition.boundedTextField,
    FieldDefinition.fixedTextField,
    FieldDefinition.textSearchVectorField,
    FieldDefinition.dateField,
    FieldDefinition.utcTimestampField,
    FieldDefinition.localTimestampField,
    FieldDefinition.fieldOfType,
    FieldDefinition.fieldColumnName,
    FieldDefinition.fieldName,
    FieldDefinition.fieldNameToString,
    FieldDefinition.fieldNameToByteString,
    FieldDefinition.fieldType,
    FieldDefinition.fieldColumnDefinition,
    FieldDefinition.fieldIsNotNull,
    FieldDefinition.fieldNullability,
    FieldDefinition.FieldNullability (NotNullField, NullableField),
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
    SelectOptions.fieldIn,
    SelectOptions.fieldNotIn,
    SelectOptions.whereBooleanExpr,
    SelectOptions.whereAnd,
    SelectOptions.whereOr,
    SelectOptions.OrderBy,
    SelectOptions.OrderByDirection,
    SelectOptions.ascendingOrder,
    SelectOptions.descendingOrder,
    SelectOptions.orderByOrderByExpr,
    SelectOptions.orderByField,
    SelectOptions.orderByColumnName,
    SelectOptions.appendOrderBy,
    SelectOptions.orderByToClause,
    SelectOptions.orderByToExpr,
    SqlType.SqlType
      ( SqlType.SqlType,
        SqlType.sqlTypeExpr,
        SqlType.sqlTypeReferenceExpr,
        SqlType.sqlTypeOid,
        SqlType.sqlTypeMaximumLength,
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
