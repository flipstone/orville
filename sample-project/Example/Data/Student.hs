module Example.Data.Student
  ( Student(..)
  , StudentId(..)
  , StudentName(..)
  ) where

import Example.Data.Major ( Major(..), MajorId(..), MajorName(..), MajorCollege(..), majorCollegeConversion)

data Student key = Student
  { studentId :: key
  , studentName :: StudentName
  , studentMajor :: MajorId
  } deriving (Show, Eq)

newtype StudentId = StudentId
  { studentIdInt :: Int
  } deriving (Show, Eq, Ord)

newtype StudentName = StudentName
  { studentNameString :: String
  } deriving (Show, Eq)



