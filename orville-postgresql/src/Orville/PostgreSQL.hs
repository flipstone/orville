{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

"Orville.PostgreSQL" is the module you will most often want to import for using
Orville. It re-exports most of the functions you need for everyday basic
operations on table entities. If you cannot find the function you need exported here,
you may be able to find it in one of the modules that re-exports more functions
for a specific area:

* "Orville.PostgreSQL.AutoMigration"
* "Orville.PostgreSQL.Execution"
* "Orville.PostgreSQL.Expr"
* "Orville.PostgreSQL.Marshall"
* "Orville.PostgreSQL.Monad"
* "Orville.PostgreSQL.OrvilleState"
* "Orville.PostgreSQL.Schema"

Of course, you can always use the table of contents for the package to see
all the exports Orville offers.

@since 1.0.0.0
-}
module Orville.PostgreSQL
  ( -- * Basic operations on entities in tables
    EntityOperations.insertEntity
  , EntityOperations.insertEntityAndReturnRowCount
  , EntityOperations.insertAndReturnEntity
  , EntityOperations.insertEntities
  , EntityOperations.insertEntitiesAndReturnRowCount
  , EntityOperations.insertAndReturnEntities
  , EntityOperations.updateEntity
  , EntityOperations.updateEntityAndReturnRowCount
  , EntityOperations.updateAndReturnEntity
  , EntityOperations.updateFields
  , EntityOperations.updateFieldsAndReturnEntities
  , EntityOperations.updateFieldsAndReturnRowCount
  , EntityOperations.deleteEntity
  , EntityOperations.deleteEntityAndReturnRowCount
  , EntityOperations.deleteAndReturnEntity
  , EntityOperations.deleteEntities
  , EntityOperations.deleteEntitiesAndReturnRowCount
  , EntityOperations.deleteAndReturnEntities
  , EntityOperations.findEntitiesBy
  , EntityOperations.findFirstEntityBy
  , EntityOperations.findEntity
  , EntityOperations.findEntities

    -- * A simple starter monad for running Orville operations
  , Orville.Orville
  , Orville.runOrville
  , Orville.runOrvilleWithState

    -- * Creating a connection pool
  , Connection.ConnectionOptions
    ( ConnectionOptions
    , connectionString
    , connectionNoticeReporting
    , connectionPoolStripes
    , connectionPoolLingerTime
    , connectionPoolMaxConnections
    )
  , Connection.createConnectionPool
  , Connection.NoticeReporting (EnableNoticeReporting, DisableNoticeReporting)
  , Connection.MaxConnections (MaxConnectionsTotal, MaxConnectionsPerStripe)
  , Connection.StripeOption (OneStripePerCapability, StripeCount)
  , Connection.Connection
  , Connection.ConnectionPool

    -- * Opening transactions and savepoints
  , Transaction.withTransaction

    -- * Types for incorporating Orville into other Monads
  , MonadOrville.MonadOrville
  , MonadOrville.withConnection_
  , MonadOrville.withConnection
  , MonadOrville.MonadOrvilleControl (liftWithConnection, liftCatch, liftMask)
  , HasOrvilleState.HasOrvilleState (askOrvilleState, localOrvilleState)
  , OrvilleState.OrvilleState
  , OrvilleState.newOrvilleState
  , OrvilleState.resetOrvilleState
  , OrvilleState.addTransactionCallback
  , OrvilleState.TransactionEvent (BeginTransaction, NewSavepoint, ReleaseSavepoint, RollbackToSavepoint, CommitTransaction, RollbackTransaction)
  , OrvilleState.Savepoint
  , OrvilleState.addSqlExecutionCallback
  , OrvilleState.setBeginTransactionExpr
  , OrvilleState.setSqlCommenterAttributes
  , OrvilleState.addSqlCommenterAttributes
  , ErrorDetailLevel.ErrorDetailLevel (ErrorDetailLevel, includeErrorMessage, includeSchemaNames, includeRowIdentifierValues, includeNonIdentifierValues)
  , ErrorDetailLevel.defaultErrorDetailLevel
  , ErrorDetailLevel.minimalErrorDetailLevel
  , ErrorDetailLevel.maximalErrorDetailLevel

    -- * Functions for defining a database schema
  , TableDefinition.TableDefinition
  , TableDefinition.mkTableDefinition
  , TableDefinition.mkTableDefinitionWithoutKey
  , TableDefinition.setTableSchema
  , TableDefinition.tableConstraints
  , TableDefinition.addTableConstraints
  , TableDefinition.tableIndexes
  , TableDefinition.addTableIndexes
  , TableDefinition.dropColumns
  , TableDefinition.columnsToDrop
  , TableDefinition.tableIdentifier
  , TableDefinition.tableName
  , TableDefinition.mkCreateTableExpr
  , TableDefinition.mkTableColumnDefinitions
  , TableDefinition.mkTablePrimaryKeyExpr
  , TableDefinition.tablePrimaryKey
  , TableDefinition.tableMarshaller
  , TableDefinition.HasKey
  , TableDefinition.NoKey
  , TableIdentifier.TableIdentifier
  , TableIdentifier.unqualifiedNameToTableId
  , TableIdentifier.tableIdUnqualifiedNameString
  , TableIdentifier.tableIdQualifiedName
  , TableIdentifier.setTableIdSchema
  , TableIdentifier.tableIdSchemaNameString
  , TableIdentifier.tableIdToString
  , ConstraintDefinition.ConstraintDefinition
  , ConstraintDefinition.uniqueConstraint
  , ConstraintDefinition.foreignKeyConstraint
  , ConstraintDefinition.foreignKeyConstraintWithOptions
  , ConstraintDefinition.ForeignKeyOptions
  , ConstraintDefinition.foreignKeyOptionsOnDelete
  , ConstraintDefinition.foreignKeyOptionsOnUpdate
  , ConstraintDefinition.defaultForeignKeyOptions
  , ConstraintDefinition.ForeignKeyAction (..)
  , ConstraintDefinition.ForeignReference (ForeignReference, localFieldName, foreignFieldName)
  , ConstraintDefinition.foreignReference
  , ConstraintDefinition.ConstraintMigrationKey (ConstraintMigrationKey, constraintKeyType, constraintKeyColumns, constraintKeyForeignTable, constraintKeyForeignColumns, constraintKeyForeignKeyOnUpdateAction, constraintKeyForeignKeyOnDeleteAction)
  , ConstraintDefinition.ConstraintKeyType (UniqueConstraint, ForeignKeyConstraint)
  , ConstraintDefinition.constraintMigrationKey
  , ConstraintDefinition.constraintSqlExpr
  , IndexDefinition.IndexDefinition
  , IndexDefinition.uniqueIndex
  , IndexDefinition.nonUniqueIndex
  , IndexDefinition.mkIndexDefinition
  , IndexDefinition.mkNamedIndexDefinition
  , IndexDefinition.IndexUniqueness (UniqueIndex, NonUniqueIndex)
  , IndexDefinition.indexCreateExpr
  , IndexDefinition.IndexCreationStrategy (Transactional, Concurrent)
  , IndexDefinition.setIndexCreationStrategy
  , IndexDefinition.indexCreationStrategy
  , PrimaryKey.PrimaryKey
  , PrimaryKey.primaryKey
  , PrimaryKey.compositePrimaryKey
  , PrimaryKey.primaryKeyPart
  , SqlMarshaller.SqlMarshaller
  , SqlMarshaller.AnnotatedSqlMarshaller
  , SqlMarshaller.annotateSqlMarshaller
  , SqlMarshaller.annotateSqlMarshallerEmptyAnnotation
  , SqlMarshaller.unannotatedSqlMarshaller
  , SqlMarshaller.mapSqlMarshaller
  , SqlMarshaller.marshallField
  , SqlMarshaller.marshallNested
  , SqlMarshaller.marshallSyntheticField
  , SqlMarshaller.marshallReadOnly
  , SqlMarshaller.marshallReadOnlyField
  , SqlMarshaller.marshallPartial
  , SqlMarshaller.marshallMaybe
  , SqlMarshaller.prefixMarshaller
  , SqlMarshaller.foldMarshallerFields
  , SqlMarshaller.collectFromField
  , SqlMarshaller.ReadOnlyColumnOption (IncludeReadOnlyColumns, ExcludeReadOnlyColumns)
  , SyntheticField.SyntheticField
  , SyntheticField.syntheticFieldExpression
  , SyntheticField.syntheticFieldAlias
  , SyntheticField.syntheticFieldValueFromSqlValue
  , SyntheticField.syntheticField
  , SyntheticField.nullableSyntheticField
  , SyntheticField.prefixSyntheticField
  , FieldDefinition.FieldDefinition
  , FieldDefinition.NotNull
  , FieldDefinition.Nullable
  , FieldDefinition.nullableField
  , FieldDefinition.asymmetricNullableField
  , FieldDefinition.convertField
  , FieldDefinition.coerceField
  , FieldDefinition.setDefaultValue
  , FieldDefinition.removeDefaultValue
  , FieldDefinition.prefixField
  , FieldDefinition.integerField
  , FieldDefinition.serialField
  , FieldDefinition.smallIntegerField
  , FieldDefinition.uuidField
  , FieldDefinition.bigIntegerField
  , FieldDefinition.bigSerialField
  , FieldDefinition.doubleField
  , FieldDefinition.booleanField
  , FieldDefinition.unboundedTextField
  , FieldDefinition.boundedTextField
  , FieldDefinition.fixedTextField
  , FieldDefinition.textSearchVectorField
  , FieldDefinition.dateField
  , FieldDefinition.utcTimestampField
  , FieldDefinition.localTimestampField
  , FieldDefinition.jsonbField
  , FieldDefinition.fieldOfType
  , FieldDefinition.fieldColumnName
  , FieldDefinition.fieldColumnReference
  , FieldDefinition.fieldName
  , FieldDefinition.setFieldName
  , FieldDefinition.fieldDescription
  , FieldDefinition.setFieldDescription
  , FieldDefinition.addUniqueConstraint
  , FieldDefinition.addForeignKeyConstraint
  , FieldDefinition.FieldName
  , FieldDefinition.stringToFieldName
  , FieldDefinition.fieldNameToString
  , FieldDefinition.fieldNameToColumnName
  , FieldDefinition.fieldNameToByteString
  , FieldDefinition.fieldType
  , FieldDefinition.fieldDefaultValue
  , FieldDefinition.fieldColumnDefinition
  , FieldDefinition.fieldIsNotNullable
  , FieldDefinition.fieldNullability
  , FieldDefinition.setField
  , (FieldDefinition..:=)
  , FieldDefinition.FieldNullability (NotNullField, NullableField)
  , DefaultValue.DefaultValue
  , DefaultValue.integerDefault
  , DefaultValue.smallIntegerDefault
  , DefaultValue.bigIntegerDefault
  , DefaultValue.integralDefault
  , DefaultValue.doubleDefault
  , DefaultValue.booleanDefault
  , DefaultValue.textDefault
  , DefaultValue.dateDefault
  , DefaultValue.currentDateDefault
  , DefaultValue.utcTimestampDefault
  , DefaultValue.currentUTCTimestampDefault
  , DefaultValue.localTimestampDefault
  , DefaultValue.currentLocalTimestampDefault
  , DefaultValue.coerceDefaultValue
  , DefaultValue.defaultValueExpression
  , DefaultValue.rawSqlDefault

    -- * Functions and operators for putting where clauses, order by clauses

  -- and limits on selects
  , SelectOptions.SelectOptions
  , SelectOptions.distinct
  , SelectOptions.groupBy
  , SelectOptions.limit
  , SelectOptions.offset
  , SelectOptions.orderBy
  , SelectOptions.where_
  , SelectOptions.emptySelectOptions
  , SelectOptions.appendSelectOptions
  , FieldDefinition.fieldEquals
  , (FieldDefinition..==)
  , FieldDefinition.fieldNotEquals
  , (FieldDefinition../=)
  , FieldDefinition.fieldGreaterThan
  , (FieldDefinition..>)
  , FieldDefinition.fieldLessThan
  , (FieldDefinition..<)
  , FieldDefinition.fieldGreaterThanOrEqualTo
  , (FieldDefinition..>=)
  , FieldDefinition.fieldLessThanOrEqualTo
  , (FieldDefinition..<=)
  , FieldDefinition.fieldLike
  , FieldDefinition.fieldLikeInsensitive
  , FieldDefinition.fieldIsNull
  , FieldDefinition.fieldIsNotNull
  , FieldDefinition.fieldIn
  , (FieldDefinition..<-)
  , FieldDefinition.fieldNotIn
  , (FieldDefinition..</-)
  , FieldDefinition.fieldTupleIn
  , FieldDefinition.fieldTupleNotIn
  , Expr.OrderByDirection
  , Expr.NullsOrder (..)
  , Expr.ascendingOrder
  , Expr.ascendingOrderWith
  , Expr.descendingOrder
  , Expr.descendingOrderWith
  , FieldDefinition.orderByField
  , Expr.orderByColumnName
  , Expr.andExpr
  , Expr.orExpr
  , (Expr..&&)
  , (Expr..||)
  , SelectOptions.selectGroupByClause
  , SelectOptions.selectOrderByClause
  , SelectOptions.selectWhereClause
  , SelectOptions.selectDistinct

    -- * Functions for defining and working with sequences
  , Sequence.sequenceNextValue
  , Sequence.sequenceCurrentValue
  , Sequence.sequenceSetValue
  , SequenceDefinition.SequenceDefinition
  , SequenceDefinition.mkSequenceDefinition
  , SequenceDefinition.setSequenceSchema
  , SequenceDefinition.sequenceIdentifier
  , SequenceDefinition.sequenceName
  , SequenceDefinition.sequenceIncrement
  , SequenceDefinition.setSequenceIncrement
  , SequenceDefinition.sequenceMinValue
  , SequenceDefinition.setSequenceMinValue
  , SequenceDefinition.sequenceMaxValue
  , SequenceDefinition.setSequenceMaxValue
  , SequenceDefinition.sequenceStart
  , SequenceDefinition.setSequenceStart
  , SequenceDefinition.sequenceCache
  , SequenceDefinition.setSequenceCache
  , SequenceDefinition.sequenceCycle
  , SequenceDefinition.setSequenceCycle
  , SequenceDefinition.mkCreateSequenceExpr
  , SequenceIdentifier.SequenceIdentifier
  , SequenceIdentifier.unqualifiedNameToSequenceId
  , SequenceIdentifier.sequenceIdUnqualifiedNameString
  , SequenceIdentifier.sequenceIdQualifiedName
  , SequenceIdentifier.setSequenceIdSchema
  , SequenceIdentifier.sequenceIdSchemaNameString
  , SequenceIdentifier.sequenceIdToString

    -- * Numeric types
  , SqlType.integer
  , SqlType.serial
  , SqlType.bigInteger
  , SqlType.bigSerial
  , SqlType.double

    -- * Textual-ish types
  , SqlType.boolean
  , SqlType.unboundedText
  , SqlType.fixedText
  , SqlType.boundedText
  , SqlType.textSearchVector
  , SqlType.uuid

    -- * Date types
  , SqlType.date
  , SqlType.timestamp

    -- * Json type
  , SqlType.jsonb

    -- * Type conversions
  , SqlType.foreignRefType
  , SqlType.convertSqlType
  , SqlType.tryConvertSqlType
  , SqlType.SqlType
    ( SqlType.SqlType
    , SqlType.sqlTypeExpr
    , SqlType.sqlTypeReferenceExpr
    , SqlType.sqlTypeOid
    , SqlType.sqlTypeMaximumLength
    , SqlType.sqlTypeToSql
    , SqlType.sqlTypeFromSql
    , SqlType.sqlTypeDontDropImplicitDefaultDuringMigrate
    )
  , Expr.QueryExpr
  , Execute.executeAndDecode
  , Execute.executeAndReturnAffectedRows
  , Execute.executeVoid
  , QueryType.QueryType (SelectQuery, InsertQuery, UpdateQuery, DeleteQuery, DDLQuery, OtherQuery)

    -- * [SqlCommenter](https://google.github.io/sqlcommenter/) support
  , SqlCommenter.SqlCommenterAttributes
  )
where

import qualified Orville.PostgreSQL.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Execution.EntityOperations as EntityOperations
import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import qualified Orville.PostgreSQL.Execution.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Execution.Sequence as Sequence
import qualified Orville.PostgreSQL.Execution.Transaction as Transaction
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall.DefaultValue as DefaultValue
import qualified Orville.PostgreSQL.Marshall.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Marshall.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType
import qualified Orville.PostgreSQL.Marshall.SyntheticField as SyntheticField
import qualified Orville.PostgreSQL.Monad.HasOrvilleState as HasOrvilleState
import qualified Orville.PostgreSQL.Monad.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Monad.Orville as Orville
import qualified Orville.PostgreSQL.OrvilleState as OrvilleState
import qualified Orville.PostgreSQL.Raw.Connection as Connection
import qualified Orville.PostgreSQL.Raw.SqlCommenter as SqlCommenter
import qualified Orville.PostgreSQL.Schema.ConstraintDefinition as ConstraintDefinition
import qualified Orville.PostgreSQL.Schema.IndexDefinition as IndexDefinition
import qualified Orville.PostgreSQL.Schema.PrimaryKey as PrimaryKey
import qualified Orville.PostgreSQL.Schema.SequenceDefinition as SequenceDefinition
import qualified Orville.PostgreSQL.Schema.SequenceIdentifier as SequenceIdentifier
import qualified Orville.PostgreSQL.Schema.TableDefinition as TableDefinition
import qualified Orville.PostgreSQL.Schema.TableIdentifier as TableIdentifier
