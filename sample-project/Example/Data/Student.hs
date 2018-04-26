module Example.Data.Student
  ( Student(..)
  , StudentId(..)
  , StudentName(..)
  , StudentMajor(..)
  ) where

data Student key = Student
  { studentId :: key
  , studentName :: StudentName
  , studentMajor :: StudentMajor
  } deriving (Show, Eq)

newtype StudentId = StudentId
  { getStudentId :: String
  } deriving (Show, Eq, Ord)

newtype StudentName = StudentName
  { getStudentName :: String
  } deriving (Show, Eq)

newtype StudentMajor = StudentMajor
  { getStudentMajor :: String
  } deriving (Show, Eq)


