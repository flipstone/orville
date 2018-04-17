{-|
Module    : Database.Orville.Internal.FieldDefintion
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Internal.FieldDefinition where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.HDBC

import Database.Orville.Internal.SqlConversion
import Database.Orville.Internal.Types

textField :: String -> Int -> FieldDefinition Text
textField name len = (name, VarText len, [], textConversion)

fixedTextField :: String -> Int -> FieldDefinition Text
fixedTextField name len = (name, Text len, [], textConversion)

dayField :: String -> FieldDefinition Day
dayField = fieldOfType Date dayConversion

utcTimeField :: String -> FieldDefinition UTCTime
utcTimeField = fieldOfType Timestamp utcTimeConversion

int32Field :: String -> FieldDefinition Int32
int32Field = fieldOfType Integer int32Conversion

int64Field :: String -> FieldDefinition Int64
int64Field = fieldOfType BigInteger int64Conversion

doubleField :: String -> FieldDefinition Double
doubleField = fieldOfType Double doubleConversion

boolField :: String -> FieldDefinition Bool
boolField = fieldOfType Boolean boolConversion

automaticIdField :: String -> FieldDefinition Int
automaticIdField = fieldOfType AutomaticId intConversion

searchVectorField :: String -> FieldDefinition Text
searchVectorField = fieldOfType TextSearchVector textConversion

nullableField :: FieldDefinition a -> FieldDefinition (Maybe a)
nullableField field = field `withFlag` Null `withConversion` nullableConversion

foreignKeyField ::
     String
  -> TableDefinition entity key
  -> FieldDefinition key
  -> FieldDefinition key
foreignKeyField name refTable refField =
  ( name
  , foreignFieldType (fieldType refField)
  , [References refTable refField]
  , fieldConversion refField)
  where
    foreignFieldType AutomaticId = ForeignId
    foreignFieldType typ = typ

-- This is an internal field for building the basic field types
-- above. It should not be exposed outside Orville
fieldOfType :: ColumnType -> SqlConversion a -> String -> FieldDefinition a
fieldOfType columnType conversion name = (name, columnType, [], conversion)

isPrimaryKey :: ColumnFlag -> Bool
isPrimaryKey PrimaryKey = True
isPrimaryKey _ = False

isNullFlag :: ColumnFlag -> Bool
isNullFlag Null = True
isNullFlag _ = False

isUninserted :: ColumnFlag -> Bool
isUninserted PrimaryKey = True
isUninserted _ = False

fieldName :: FieldDefinition a -> String
fieldName (name, _, _, _) = name

escapedFieldName :: FieldDefinition a -> String
escapedFieldName field = "\"" ++ fieldName field ++ "\""

fieldType :: FieldDefinition a -> ColumnType
fieldType (_, typ, _, _) = typ

isPrimaryKeyField :: FieldDefinition a -> Bool
isPrimaryKeyField (_, _, flags, _) = any isPrimaryKey flags

withFlag :: FieldDefinition a -> ColumnFlag -> FieldDefinition a
withFlag (name, typ, flags, conversion) newFlag =
  (name, typ, newFlag : flags, conversion)

withName :: FieldDefinition a -> String -> FieldDefinition a
withName (_, typ, flags, conversion) newName =
  (newName, typ, flags, conversion)

withConversion ::
     FieldDefinition a
  -> (SqlConversion a -> SqlConversion b)
  -> FieldDefinition b
withConversion (name, typ, flags, aConversion) mapConversion =
  (name, typ, flags, mapConversion aConversion)

isUninsertedField :: FieldDefinition a -> Bool
isUninsertedField (_, _, flags, _) = any isUninserted flags

withPrefix :: FieldDefinition a -> String -> FieldDefinition a
withPrefix f@(name, _, _, _) prefix = f `withName` (prefix ++ "_" ++ name)

fieldConversion :: FieldDefinition a -> SqlConversion a
fieldConversion (_, _, _, conversion) = conversion

fieldToSqlValue :: FieldDefinition a -> a -> SqlValue
fieldToSqlValue = convertToSql . fieldConversion

fieldFromSqlValue :: FieldDefinition a -> SqlValue -> Maybe a
fieldFromSqlValue = convertFromSql . fieldConversion
