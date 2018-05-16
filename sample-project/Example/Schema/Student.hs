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

import qualified Database.Orville as O

import Example.Data.Student (Student(..), StudentId(..), StudentName(..) )
import Example.Data.Major (Major(..), MajorId(..), MajorName(..), MajorCollege(..), collegeMajorToText, textToCollegeMajor)

majorTable :: O.TableDefinition Major MajorId
majorTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "majors"
    , O.tblPrimaryKey = majorIdField
    , O.tblMapper
       =
        Major <$> O.attrField majorId majorIdField <*>
        O.attrField majorName majorNameField <*>
        O.attrField majorCollege majorCollegeField
    , O.tblGetKey = majorId
    , O.tblSetKey = \key entity -> entity {majorId = key}
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

majorIdField :: O.FieldDefinition MajorId
majorIdField = 
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.sqlConversionVia majorIdInt MajorId

majorNameField :: O.FieldDefinition MajorName
majorNameField =
  O.textField "name" 255 `O.withConversion`
  O.sqlConversionVia majorNameText MajorName

majorCollegeField :: O.FieldDefinition MajorCollege
majorCollegeField =
  O.textField "college" 255 `O.withConversion`
  O.sqlConversionVia collegeMajorToText textToCollegeMajor


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
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.sqlConversionVia studentIdInt StudentId

studentNameField :: O.FieldDefinition StudentName
studentNameField =
  O.textField "name" 255 `O.withConversion`
  O.sqlConversionVia studentNameText StudentName

studentMajorField :: O.FieldDefinition MajorId
studentMajorField = 
  O.foreignKeyField "major" majorTable majorIdField
  







