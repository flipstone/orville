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
import Example.Data.Major (Major(..), MajorId(..), MajorName(..), MajorCollege(..), majorCollegeConversion)

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
  O.withConversion 
  (O.withFlag (O.automaticIdField "id") O.PrimaryKey) 
  (O.sqlConversionVia studentIdInt StudentId)

studentNameField :: O.FieldDefinition StudentName
studentNameField =
  ( "name"
  , O.VarText 255
  , []
  , O.sqlConversionVia studentNameString StudentName O.sqlConvertible)

studentMajorField :: O.FieldDefinition MajorId
studentMajorField =
  ( "major"
  , O.ForeignId
  , []
  , O.sqlConversionVia majorIdInt MajorId O.sqlConvertible)


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
  O.withConversion 
  (O.withFlag (O.automaticIdField "id") O.PrimaryKey) 
  (O.sqlConversionVia majorIdInt MajorId)

majorNameField :: O.FieldDefinition MajorName
majorNameField =
  ( "name"
  , O.VarText 255
  , []
  , O.sqlConversionVia majorNameString MajorName O.sqlConvertible)

majorCollegeField :: O.FieldDefinition MajorCollege
majorCollegeField =
  ( "college"
  , O.VarText 255
  , []
  , majorCollegeConversion)
