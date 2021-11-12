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
    TableDefinition.tableConstraints,
    TableDefinition.addTableConstraints,
    TableDefinition.tableIndexes,
    TableDefinition.addTableIndexes,
    TableDefinition.dropColumns,
    TableDefinition.columnsToDrop,
    TableDefinition.tableIdentifier,
    TableDefinition.tableName,
    TableDefinition.mkCreateTableExpr,
    TableDefinition.mkTableColumnDefinitions,
    TableDefinition.mkTablePrimaryKeyExpr,
    TableDefinition.tablePrimaryKey,
    TableDefinition.tableMarshaller,
    TableDefinition.HasKey,
    TableDefinition.NoKey,
    TableIdentifier.TableIdentifier,
    TableIdentifier.unqualifiedNameToTableId,
    TableIdentifier.tableIdUnqualifiedNameString,
    TableIdentifier.tableIdQualifiedName,
    TableIdentifier.setTableIdSchema,
    TableIdentifier.tableIdSchemaNameString,
    TableIdentifier.tableIdToString,
    ConstraintDefinition.ConstraintDefinition,
    ConstraintDefinition.uniqueConstraint,
    ConstraintDefinition.foreignKeyConstraint,
    ConstraintDefinition.ForeignReference (localFieldName, foreignFieldName),
    ConstraintDefinition.foreignReference,
    ConstraintDefinition.ConstraintMigrationKey (ConstraintMigrationKey, constraintKeyType, constraintKeyColumns, constraintKeyForeignTable, constraintKeyForeignColumns),
    ConstraintDefinition.ConstraintKeyType (UniqueConstraint, ForeignKeyConstraint),
    ConstraintDefinition.constraintMigrationKey,
    ConstraintDefinition.constraintSqlExpr,
    IndexDefinition.IndexDefinition,
    IndexDefinition.uniqueIndex,
    IndexDefinition.nonUniqueIndex,
    IndexDefinition.mkIndexDefinition,
    IndexDefinition.IndexUniqueness (UniqueIndex, NonUniqueIndex),
    IndexDefinition.IndexMigrationKey (IndexMigrationKey, indexKeyUniqueness, indexKeyColumns),
    IndexDefinition.indexMigrationKey,
    IndexDefinition.indexCreateExpr,
    PrimaryKey.PrimaryKey,
    PrimaryKey.primaryKey,
    PrimaryKey.compositePrimaryKey,
    PrimaryKey.primaryKeyPart,
    SqlMarshaller.SqlMarshaller,
    SqlMarshaller.marshallField,
    SqlMarshaller.marshallSyntheticField,
    SqlMarshaller.marshallReadOnly,
    SqlMarshaller.marshallReadOnlyField,
    SqlMarshaller.marshallPartial,
    SqlMarshaller.marshallMaybe,
    SqlMarshaller.foldMarshallerFields,
    SqlMarshaller.collectFromField,
    SqlMarshaller.ReadOnlyColumnOption (IncludeReadOnlyColumns, ExcludeReadOnlyColumns),
    SyntheticField.SyntheticField,
    SyntheticField.syntheticFieldExpression,
    SyntheticField.syntheticFieldAlias,
    SyntheticField.syntheticFieldValueFromSqlValue,
    SyntheticField.syntheticField,
    SyntheticField.nullableSyntheticField,
    FieldDefinition.FieldDefinition,
    FieldDefinition.NotNull,
    FieldDefinition.Nullable,
    FieldDefinition.nullableField,
    FieldDefinition.asymmetricNullableField,
    FieldDefinition.convertField,
    FieldDefinition.coerceField,
    FieldDefinition.setDefaultValue,
    FieldDefinition.removeDefaultValue,
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
    FieldDefinition.FieldName,
    FieldDefinition.stringToFieldName,
    FieldDefinition.fieldNameToString,
    FieldDefinition.fieldNameToColumnName,
    FieldDefinition.fieldNameToByteString,
    FieldDefinition.fieldType,
    FieldDefinition.fieldDefaultValue,
    FieldDefinition.fieldColumnDefinition,
    FieldDefinition.fieldIsNotNullable,
    FieldDefinition.fieldNullability,
    FieldDefinition.FieldNullability (NotNullField, NullableField),
    DefaultValue.DefaultValue,
    DefaultValue.integerDefault,
    DefaultValue.smallIntegerDefault,
    DefaultValue.bigIntegerDefault,
    DefaultValue.integralDefault,
    DefaultValue.doubleDefault,
    DefaultValue.booleanDefault,
    DefaultValue.textDefault,
    DefaultValue.dateDefault,
    DefaultValue.currentDateDefault,
    DefaultValue.utcTimestampDefault,
    DefaultValue.currentUTCTimestampDefault,
    DefaultValue.localTimestampDefault,
    DefaultValue.currentLocalTimestampDefault,
    DefaultValue.coerceDefaultValue,
    DefaultValue.defaultValueExpression,
    DefaultValue.rawSqlDefault,
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
    SelectOptions.fieldIsNull,
    SelectOptions.fieldIsNotNull,
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
        SqlType.sqlTypeFromSql,
        SqlType.sqlTypeDontDropImplicitDefaultDuringMigrate
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
import qualified Orville.PostgreSQL.Internal.ConstraintDefinition as ConstraintDefinition
import qualified Orville.PostgreSQL.Internal.DefaultValue as DefaultValue
import qualified Orville.PostgreSQL.Internal.EntityOperations as EntityOperations
import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Internal.IndexDefinition as IndexDefinition
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.Orville as Orville
import qualified Orville.PostgreSQL.Internal.OrvilleState as OrvilleState
import qualified Orville.PostgreSQL.Internal.PrimaryKey as PrimaryKey
import qualified Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlType as SqlType
import qualified Orville.PostgreSQL.Internal.SyntheticField as SyntheticField
import qualified Orville.PostgreSQL.Internal.TableDefinition as TableDefinition
import qualified Orville.PostgreSQL.Internal.TableIdentifier as TableIdentifier
import qualified Orville.PostgreSQL.Internal.Transaction as Transaction
