module Example.SchemaStudent
  ( module Example.SchemaStudent
  , module Example.Schema.Student
  ) where

import qualified Database.Orville as O

import Example.Schema.Student

studentSchema :: O.SchemaDefinition
studentSchema = [O.Table studentTable, O.Table majorTable]
