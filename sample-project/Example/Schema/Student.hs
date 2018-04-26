module Example.Schema.Student
  ( studentTable
  , studentIdField
  , studentNameField
  , studentMajorField
  ) where

import qualified Database.Orville as O
--import qualified Database.Orville.Internal as OIn

import Example.Data.Student (Student(..), StudentId(..), StudentName(..), StudentMajor(..))

studentTable :: O.TableDefinition Student StudentId
studentTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "students"
    , O.tblPrimaryKey = studentIdField
    , O.tblMapper
       =
        Student <$> O.attrField studentId studentIdField <*>
        O.attrField studentName studentNameField <*>
        O.attrField studentMajor studentMajorField
    , O.tblGetKey = studentId
    , O.tblSetKey = \key entity -> entity {studentId = key}
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

studentIdField :: O.FieldDefinition StudentId
studentIdField =
  ( "id"
  , O.AutomaticId
  , [O.PrimaryKey]
  , O.sqlConversionVia getStudentId StudentId O.sqlConvertible)

studentNameField :: O.FieldDefinition StudentName
studentNameField =
  ( "name"
  , O.VarText 255
  , []
  , O.sqlConversionVia getStudentName StudentName O.sqlConvertible)

studentMajorField :: O.FieldDefinition StudentMajor
studentMajorField =
  ( "major"
  , O.VarText 255
  , [O.Null]
  , O.sqlConversionVia getStudentMajor StudentMajor O.sqlConvertible)
