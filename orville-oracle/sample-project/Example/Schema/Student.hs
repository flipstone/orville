module Example.Schema.Student
  ( studentTable
  , studentIdField
  , studentNameField
  , studentMajorField
  , majorTable
  , majorIdField
  , majorNameField
  , majorCollegeField
  ) where

import qualified Database.Orville.Oracle as O

import Example.Data.Major
  ( Major(..)
  , MajorCollege(..)
  , MajorId(..)
  , MajorName(..)
  , collegeMajorToText
  , textToCollegeMajor
  )
import Example.Data.Student (Student(..), StudentId(..), StudentName(..))

majorTable :: O.TableDefinition (Major MajorId) (Major ()) MajorId
majorTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "majors"
    , O.tblPrimaryKey = majorIdField
    , O.tblMapper =
        Major <$> O.readOnlyField majorIdField <*>
        O.attrField majorName majorNameField <*>
        O.attrField majorCollege majorCollegeField
    , O.tblGetKey = majorId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

majorIdField :: O.FieldDefinition MajorId
majorIdField =
  O.int32Field "ID" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType majorIdInt MajorId

majorNameField :: O.FieldDefinition MajorName
majorNameField =
  O.textField "NAME" 255 `O.withConversion`
  O.convertSqlType majorNameText MajorName

majorCollegeField :: O.FieldDefinition MajorCollege
majorCollegeField =
  O.textField "COLLEGE" 255 `O.withConversion`
  O.convertSqlType collegeMajorToText textToCollegeMajor

studentTable :: O.TableDefinition (Student StudentId) (Student ()) StudentId
studentTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "students"
    , O.tblPrimaryKey = studentIdField
    , O.tblMapper =
        Student <$> O.readOnlyField studentIdField <*>
        O.attrField studentName studentNameField <*>
        O.attrField studentMajor studentMajorField
    , O.tblGetKey = studentId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

studentIdField :: O.FieldDefinition StudentId
studentIdField =
  O.int32Field "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType studentIdInt StudentId

studentNameField :: O.FieldDefinition StudentName
studentNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType studentNameText StudentName

studentMajorField :: O.FieldDefinition MajorId
studentMajorField = O.foreignKeyField "major" majorTable majorIdField
