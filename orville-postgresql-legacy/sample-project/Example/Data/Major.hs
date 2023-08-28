module Example.Data.Major
  ( Major(..)
  , MajorId(..)
  , MajorName(..)
  , MajorCollege(..)
  , collegeMajorToText
  , textToCollegeMajor
  ) where

import Data.Int (Int32)
import Data.Text (Text, pack, unpack)

data Major key = Major
  { majorId :: key
  , majorName :: MajorName
  , majorCollege :: MajorCollege
  } deriving (Show, Eq)

newtype MajorId = MajorId
  { majorIdInt :: Int32
  } deriving (Show, Eq, Ord)

newtype MajorName = MajorName
  { majorNameText :: Text
  } deriving (Show, Eq)

data MajorCollege
  = NaturalScience
  | LiberalArts
  | Other
  deriving (Show, Eq)

collegeMajorToText :: MajorCollege -> Text
collegeMajorToText col =
  case col of
    NaturalScience -> pack "Natural Science"
    LiberalArts -> pack "Liberal Arts"
    Other -> pack "Other"

textToCollegeMajor :: Text -> MajorCollege
textToCollegeMajor txt =
  case unpack txt of
    "Natural Science" -> NaturalScience
    "Liberal Arts" -> LiberalArts
    _ -> Other
