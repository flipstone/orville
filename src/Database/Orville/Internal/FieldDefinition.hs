module Database.Orville.Internal.FieldDefinition where

import            Database.Orville.Internal.Types
import            Database.Orville.Internal.QueryKey

isPrimaryKey :: ColumnFlag -> Bool
isPrimaryKey PrimaryKey = True
isPrimaryKey _ = False

isNullFlag :: ColumnFlag -> Bool
isNullFlag Null = True
isNullFlag _ = False

isUninserted :: ColumnFlag -> Bool
isUninserted PrimaryKey = True
isUninserted (InsertDefault _) = True
isUninserted _ = False

fieldName :: FieldDefinition -> String
fieldName (name, _, _) = name

escapedFieldName :: FieldDefinition -> String
escapedFieldName field = "\"" ++ fieldName field ++ "\""

fieldType :: FieldDefinition -> ColumnType
fieldType (_,typ,_) = typ

isPrimaryKeyField :: FieldDefinition -> Bool
isPrimaryKeyField (_, _, flags) = any isPrimaryKey flags

withFlag :: FieldDefinition -> ColumnFlag -> FieldDefinition
withFlag (name, typ, flags) newFlag = (name, typ, newFlag : flags)

withName :: FieldDefinition -> String -> FieldDefinition
withName (_, typ, flags) newName = (newName, typ, flags)

isUninsertedField :: FieldDefinition -> Bool
isUninsertedField (_, _, flags) = any isUninserted flags

