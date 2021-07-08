{- |
Module    : Orville.PostgreSQL.Raw
Copyright : Flipstone Technology Partners 2020-2021
License   : MIT
-}
module Orville.PostgreSQL
  ( EntityOperations.insertEntity,
    EntityOperations.insertEntities,
    EntityOperations.updateEntity,
    EntityOperations.findEntitiesBy,
    EntityOperations.findFirstEntityBy,
    EntityOperations.findEntity,
    Connection.createConnectionPool,
    TableDefinition.TableDefinition,
    TableDefinition.mkTableDefiniton,
    PrimaryKey.PrimaryKey,
    PrimaryKey.primaryKey,
    PrimaryKey.compositePrimaryKey,
    SqlMarshaller.SqlMarshaller,
    SqlMarshaller.marshallField,
    FieldDefinition.FieldDefinition,
    FieldDefinition.NotNull,
    FieldDefinition.Nullable,
    FieldDefinition.nullableField,
    FieldDefinition.asymmetricNullableField,
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
    FieldDefinition.fieldOfType,
    Orville.Orville,
    Orville.runOrville,
    MonadOrville.MonadOrville,
    MonadOrville.withConnection,
    MonadOrville.MonadOrvilleControl (liftWithConnection),
    MonadOrville.HasOrvilleState (askOrvilleState, localOrvilleState),
    MonadOrville.OrvilleState,
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

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.EntityOperations as EntityOperations
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.Orville as Orville
import qualified Orville.PostgreSQL.Internal.PrimaryKey as PrimaryKey
import qualified Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlType as SqlType
import qualified Orville.PostgreSQL.Internal.TableDefinition as TableDefinition
