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

import qualified Database.Orville.PostgreSQL as O

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
    , O.tblPrimaryKey = O.primaryKey majorIdField
    , O.tblMapper =
        Major <$> O.readOnlyField majorIdField <*>
        O.attrField majorName majorNameField <*>
        O.attrField majorCollege majorCollegeField
    , O.tblGetKey = majorId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

majorIdField :: O.FieldDefinition O.NotNull MajorId
majorIdField =
  O.automaticIdField "id" `O.withConversion`
  O.convertSqlType majorIdInt MajorId

majorNameField :: O.FieldDefinition O.NotNull MajorName
majorNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType majorNameText MajorName

majorCollegeField :: O.FieldDefinition O.NotNull MajorCollege
majorCollegeField =
  O.textField "college" 255 `O.withConversion`
  O.convertSqlType collegeMajorToText textToCollegeMajor

studentTable :: O.TableDefinition (Student StudentId) (Student ()) StudentId
studentTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "students"
    , O.tblPrimaryKey = O.primaryKey studentIdField
    , O.tblMapper =
        Student <$> O.readOnlyField studentIdField <*>
        O.attrField studentName studentNameField <*>
        O.attrField studentMajor studentMajorField
    , O.tblGetKey = studentId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

studentIdField :: O.FieldDefinition O.NotNull StudentId
studentIdField =
  O.automaticIdField "id" `O.withConversion`
  O.convertSqlType studentIdInt StudentId

studentNameField :: O.FieldDefinition O.NotNull StudentName
studentNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType studentNameText StudentName

studentMajorField :: O.FieldDefinition O.NotNull MajorId
studentMajorField = O.foreignKeyField "major" majorTable majorIdField
