{-|
Module    : Database.Orville.Internal.FieldDefintion
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Internal.FieldDefinition where

import Database.HDBC

import Database.Orville.Internal.SqlConversion
import Database.Orville.Internal.Types

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
