module Example.Data.Major
  ( Major(..)
  , MajorId(..)
  , MajorName(..)
  , MajorCollege(..)
  , College(..)
  ) where

data Major key = Major
  { majorId :: key
  , majorName :: MajorName
  , majorCollege :: MajorCollege
  } deriving (Show, Eq)

newtype MajorId = MajorId
  { getMajorId :: String
  } deriving (Show, Eq)

newtype MajorName = MajorName
  { getMajorName :: String
  } deriving (Show, Eq)

newtype MajorCollege = MajorCollege
  { getStudentMajor :: College
  } deriving (Show, Eq)

data College = NaturalScience | LiberalArts deriving (Show, Eq)





