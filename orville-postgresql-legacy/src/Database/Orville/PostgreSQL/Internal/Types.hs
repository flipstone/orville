{-|
Module    : Database.Orville.PostgreSQL.Internal.Types
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Database.Orville.PostgreSQL.Internal.Types where

import Control.Exception
import Control.Monad (join)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Typeable
import Database.HDBC

import qualified Data.Time as Time

import Database.Orville.PostgreSQL.Internal.Expr
import Database.Orville.PostgreSQL.Internal.QueryKey
import Database.Orville.PostgreSQL.Internal.SqlType

type Record = Int

type CreatedAt = Time.UTCTime

type UpdatedAt = Time.UTCTime

type OccurredAt = Time.UTCTime

data ColumnFlag
  = forall a. ColumnDefault a => Default a
  | Unique
  | forall readEntity writeEntity key nullability. References (TableDefinition readEntity writeEntity key)
                                                              (FieldDefinition nullability key)
  | ColumnDescription String
  | AssignedByDatabase

class ColumnDefault a where
  toColumnDefaultSql :: a -> String

data Now =
  Now

instance ColumnDefault [Char] where
  toColumnDefaultSql s = "'" ++ s ++ "'"

instance ColumnDefault Now where
  toColumnDefaultSql _ = "(now() at time zone 'utc')"

instance ColumnDefault Integer where
  toColumnDefaultSql val = show val

instance ColumnDefault Bool where
  toColumnDefaultSql True = "true"
  toColumnDefaultSql False = "false"

-- | 'Nullable' is a values-less type used to track that a 'FieldDefinition'
-- represents a field that is marked nullable in the database schema. See the
-- 'Nullability' type for the value-level representation of field nullability.
data Nullable

-- | 'NotNull is a values-less type used to track that a 'FieldDefinition'
-- represents a field that is marked not-null in the database schema.  See the
-- 'Nullability' type for the value-level representation of field nullability.
data NotNull

-- | 'Nullability' represents whether a field will be marked as 'NULL' or 'NOT
-- NULL' in the database schema. It is a GADT so that the value constructors
-- can be used to record this knowledge in the type system as well. This allows
-- functions that work only on 'Nullable' or 'NotNull' fields to indicate this
-- in their type signatures as appropriate.
data Nullability nullability where
  Nullable :: Nullability Nullable
  NotNull  :: Nullability NotNull

-- | A 'NullabilityCheck' is returned by the 'checkNullability' function, which
-- can be used when a function works on both 'Nullable' and 'NotNull' functions
-- but needs to deal with each type of field separately. It adds wrapper
-- constructors around the 'FieldDefinition' that you can pattern match on to
-- then work with a concrete 'Nullable' or 'NotNull' field.
data NullabilityCheck a
  = NullableField (FieldDefinition Nullable a)
  | NotNullField (FieldDefinition NotNull a)

-- | Resolves the 'nullablity' of a field to a concrete type based on its
-- 'fieldNullability'. You can do this directly by pattern matching on the
-- value of 'fieldNullability' if you have the GADTs extension turned on, but
-- this function will do that for you so you don't need to turn GADTs on.
checkNullability :: FieldDefinition nullability a -> NullabilityCheck a
checkNullability field =
  case fieldNullability field of
    Nullable -> NullableField field
    NotNull  -> NotNullField field

data FieldDefinition nullability a =
  FieldDefinition
    { fieldName        :: String
    , fieldType        :: SqlType a
    , fieldFlags       :: [ColumnFlag]
    , fieldNullability :: Nullability nullability
    }

data SomeField =
  forall nullability a.
    SomeField (FieldDefinition nullability a)

instance QueryKeyable (FieldDefinition nullability a) where
  queryKey field = QKField $ fieldName field

data FieldUpdate = FieldUpdate
  { fieldUpdateField :: SomeField
  , fieldUpdateValue :: SqlValue
  }

data TableComment = TableComment
  { tcWhat :: String
  , tcWhen :: (Int, Int, Int)
  , tcWho :: String
  }

newtype TableComments a =
  TableComments (Writer [TableComment] a)
  deriving (Functor, Applicative, Monad)

runComments :: TableComments a -> [TableComment]
runComments (TableComments commenter) = snd (runWriter commenter)

noComments :: TableComments ()
noComments = return ()

say :: String -> (Int, Int, Int) -> String -> TableComments ()
say msg msgDate commenter =
  TableComments $ writer ((), [TableComment msg msgDate commenter])

data FromSqlError
  = RowDataError !RowDataErrorDetails
    -- ^ Captures a failure in the translation of a SQL value from a particular
    -- field to it's corresponding Haskell values.
  | MissingColumn !MissingColumnDetails
    -- ^ An expected column was not returned by the database
  | ConversionError !ConversionErrorDetails
    -- ^ A conversion between haskell representations failed at a point where
    -- we don't know what column the value came from. This is the case when
    -- using the 'partialMap' combinator.
  deriving Typeable

instance Show FromSqlError where
  show = showFromSqlErrorMinimal

data RowDataErrorDetails =
  RowDataErrorDetails
    { rowErrorReason :: !RowDataErrorReason
    , rowErrorColumnName :: !String
    -- ^ Column name for the erroneous value
    , rowErrorPrimaryKeys :: ![(String, SqlValue)]
    -- ^ Primary keys. Empty if not known
    } deriving Typeable

data MissingColumnDetails =
  MissingColumnDetails
    { missingColumn :: !String
    , actualColumns :: ![String]
    }

data ConversionErrorDetails =
  ConversionErrorDetails
    { convErrorReason :: !String
    , convErrorPrimaryKeys :: ![(String, SqlValue)]
    -- ^ Primary key value(s). Empty if not known
    }

simpleConversionError :: String -> FromSqlError
simpleConversionError err =
  ConversionError ConversionErrorDetails { convErrorReason = err
                                         , convErrorPrimaryKeys = []
                                         }

-- | Shows the error in a way that should not contain any potentially sensitive
-- data. This is used for the 'Show' instance.
showFromSqlErrorMinimal :: FromSqlError -> String
showFromSqlErrorMinimal err =
  case err of
    RowDataError details ->
      "Error decoding data from column '" <> rowErrorColumnName details <> "'"
    MissingColumn details -> showMissingColumnDetails details
    ConversionError details -> convErrorReason details

-- | Shows the error in a way appropriate for logging within an application.
-- The resulting string contains information that is useful for debugging but
-- is potentially undesirable to expose outside of the application (such as
-- primary key values).
showFromSqlErrorForLogging :: FromSqlError -> String
showFromSqlErrorForLogging err =
  case err of
    RowDataError details ->
      concat
        [ "Error decoding data from column "
        , "'" <> rowErrorColumnName details <> "': "
        , showReason (rowErrorReason details)
        , showPrimaryKey (rowErrorPrimaryKeys details)
        ]
    MissingColumn details -> showMissingColumnDetails details
    ConversionError details ->
      convErrorReason details
        <> showPrimaryKey (convErrorPrimaryKeys details)
  where
    showReason reason =
      case reason of
        TypeMismatch expected actual ->
          "expected " <> expected <> " but got " <> actual
        IntegralOutOfBounds lower upper actual ->
          "expected an integral between " <> show lower
          <> " and " <> show upper <> " but got " <> show actual
        DecodingFailure failure -> failure

    showPrimaryKey [] = ""
    showPrimaryKey keyValues = " (" <> keysStr <> ")"
      where
        keysStr = List.intercalate ", " $ map showKeyValue keyValues
        showKeyValue (key, sqlValue) = key <> ": " <> showSqlValue sqlValue
        -- removes the data constructor prefix
        showSqlValue sqlValue = drop 1 . dropWhile (not . Char.isSpace)
                              $ show sqlValue

showMissingColumnDetails :: MissingColumnDetails -> String
showMissingColumnDetails details =
  concat
    [ "Column "
    , "'" <> missingColumn details <> "' "
    , "not found in result set. Actual Columns: "
    , "'" <> List.intercalate "', '" (actualColumns details) <> "'"
    ]

instance Exception FromSqlError

data FromSql a = FromSql
  { fromSqlSelects :: [SelectForm]
  , runFromSql :: [(String, SqlValue)] -> Either FromSqlError a
  }

instance Functor FromSql where
  fmap f fA = fA {runFromSql = \values -> f <$> runFromSql fA values}

instance Applicative FromSql where
  pure a = FromSql [] (\_ -> pure a)
  fF <*> fA =
    FromSql
      { fromSqlSelects = fromSqlSelects fF ++ fromSqlSelects fA
      , runFromSql = \values -> runFromSql fF values <*> runFromSql fA values
      }

getColumn :: SelectForm -> FromSql SqlValue
getColumn selectForm =
  FromSql
    { fromSqlSelects = [selectForm]
    , runFromSql =
        \values -> do
          let colName = unescapedName $ selectFormOutput selectForm
          case lookup colName values of
            Just sqlValue -> pure sqlValue
            Nothing ->
              throwError $
              MissingColumn
                MissingColumnDetails
                  { missingColumn = colName
                  , actualColumns = map fst values
                  }
    }

joinFromSqlError :: FromSql (Either FromSqlError a) -> FromSql a
joinFromSqlError fE =
  fE {runFromSql = \columns -> join $ runFromSql fE columns}

newtype ToSql a b = ToSql
  { unToSql :: ReaderT a (State [SqlValue]) b
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState [SqlValue]
             , MonadReader a
             )

runToSql :: ToSql a b -> a -> [SqlValue]
runToSql tosql a = reverse $ execState (runReaderT (unToSql tosql) a) []

getComponent :: (entity -> a) -> ToSql a () -> ToSql entity ()
getComponent getComp (ToSql serializer) =
  ToSql (withReaderT getComp serializer)

{-|
 Migration Guide: 'TableDefinition' can now be imported from
 'Orville.PostgreSQL'. The order of the type parameters has changed from
 @TableDefinition readEnity writeEntity key@ to @TableDefinition key
 writeEntity readEntity@. In the new Orville tables without primary keys are
 supported, so the @key@ parameter must now be instantiated as either @HasKey
 keyType@ or @NoKey@.

 A 'TableDefinition' is the center of the Orville universe. A 'TableDefinition'
 defines the structure of a table in the database and associates it with a Haskell
 datatype, usually a Haskell record type. The 'TableDefinition' must specify how
 the Haskell type is converted to and from the database schema, as as well as
 provide same basic utility functions required by Orville for interacting with
 the Haskell datatype.

  Usually you will use 'TableParams' to construct a 'TableDefinition' in a more
  concise way. This type is provided as an escape hatch for any situations where
  'TableParams' is too restrictive for the sql mapping required by a type.
 -}
data TableDefinition readEntity writeEntity key = TableDefinition
  { tableName :: String
      -- ^ The name of the table in the database.
  , tableFields :: [SomeField]
      -- ^ A list of field definitions defining the table structure
  , tableSafeToDelete :: [String]
      -- ^ A list of any columns that may be deleted from the table by Orville.
      -- (Orville will never delete a column without being told it is safe)
  , tablePrimaryKey :: PrimaryKey key
      -- ^ The statically typed field definition that is the primary key. Currently
      -- this field must still by listed in `tableFields`
  , tableFromSql :: FromSql readEntity
      -- ^ A definition of how to convert the haskell type from a sql row
  , tableToSql :: ToSql writeEntity ()
      -- ^ A function to set the key on the entity
  , tableGetKey :: readEntity -> key
      -- ^ A function to get the key on the entity
  , tableComments :: TableComments ()
      -- ^ Any comments that might be interesting for developers to see. These
      -- comments will get printed in the log if there is an erro while attempting
      -- to migrate the table.
  }

data PrimaryKey key
  = PrimaryKey (PrimaryKeyPart key) [PrimaryKeyPart key]

data PrimaryKeyPart key =
  forall part. PrimaryKeyPart (key -> part) (FieldDefinition NotNull part)

instance QueryKeyable (TableDefinition readEntity writeEntity key) where
  queryKey = QKTable . tableName

data SchemaItem
  = forall readEntity writeEntity key. Table (TableDefinition readEntity writeEntity key)
  | DropTable String
  | Index IndexDefinition
  | DropIndex String
  | Constraint ConstraintDefinition
  | DropConstraint String
                   String
  | Sequence SequenceDefinition
  | DropSequence String

instance Show SchemaItem where
  show (Table tableDef) = "Table <" ++ tableName tableDef ++ " definition>"
  show (DropTable name) = "DropTable " ++ show name
  show (Index indexDef) = "Index (" ++ show indexDef ++ ")"
  show (DropIndex name) = "DropIndex " ++ show name
  show (Constraint cons) = "Constraint (" ++ show cons ++ ")"
  show (DropConstraint name table) =
    "DropConstraint " ++ show name ++ " " ++ show table
  show (Sequence name) = "Sequence " ++ show name
  show (DropSequence name) = "DropSequence " ++ show name

type SchemaDefinition = [SchemaItem]

data IndexDefinition = IndexDefinition
  { indexName :: String
  , indexUnique :: Bool
  , indexTable :: String
  , indexBody :: String
  } deriving (Eq, Show)

data ConstraintDefinition = ConstraintDefinition
  { constraintName :: String
  , constraintTable :: String
  , constraintBody :: String
  } deriving (Eq, Show)

data SequenceDefinition = SequenceDefinition
  { sequenceName :: String
  , sequenceIncrement :: Maybe Int
  , sequenceMinValue :: Maybe Int
  , sequenceMaxValue :: Maybe Int
  , sequenceStart :: Maybe Int
  , sequenceCache :: Maybe Int
  , sequenceCycle :: Bool
  } deriving (Eq, Show)
