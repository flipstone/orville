module Example.Data.Major
  ( Major(..)
  , MajorId(..)
  , MajorName(..)
  , MajorCollege(..)
  , collegeMajorToText
  , textToCollegeMajor
  ) where

import Data.Text (Text, pack, unpack)

import qualified Database.Orville as O

data Major key = Major
  { majorId :: key
  , majorName :: MajorName
  , majorCollege :: MajorCollege
  } deriving (Show, Eq)

newtype MajorId = MajorId
  { majorIdInt :: Int
  } deriving (Show, Eq)

newtype MajorName = MajorName
  { majorNameText :: Text
  } deriving (Show, Eq)

data MajorCollege = NaturalScience | LiberalArts | Other deriving (Show, Eq)

collegeMajorToText :: MajorCollege -> Text
collegeMajorToText col = case col of
  NaturalScience -> pack "Natural Science"
  LiberalArts    -> pack "Liberal Arts"

textToCollegeMajor :: Text -> MajorCollege
textToCollegeMajor txt = case unpack txt of
  "Natural Science" -> NaturalScience
  "Liberal Arts"    -> LiberalArts
  _                 -> Other







