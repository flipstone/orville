module Example.SchemaStudent
  ( module Example.SchemaStudent
  , module Example.Schema.Student
  ) where

import qualified Database.Orville.Oracle as O

import Example.Schema.Student

studentSchema :: O.SchemaDefinition
studentSchema = [O.Table majorTable, O.Table studentTable]
