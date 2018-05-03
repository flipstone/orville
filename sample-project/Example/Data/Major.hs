module Example.Data.Major
  ( Major(..)
  , MajorId(..)
  , MajorName(..)
  , MajorCollege(..)
  , majorCollegeConversion
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
  { majorNameString :: String
  } deriving (Show, Eq)

data MajorCollege = NaturalScience | LiberalArts deriving (Show, Eq)

majorCollegeConversion :: O.SqlConversion MajorCollege
majorCollegeConversion = O.maybeSqlConversionVia collegeMajorToText maybeCollegeMajor O.textConversion
  where
    collegeMajorToText :: MajorCollege -> Text
    collegeMajorToText col = case col of
      NaturalScience -> pack "Natural Science"
      LiberalArts    -> pack "Liberal Arts"

    maybeCollegeMajor :: Text -> Maybe MajorCollege
    maybeCollegeMajor txt = case unpack txt of
      "Natural Science" -> Just NaturalScience
      "Liberal Arts"    -> Just LiberalArts
      _                 -> Nothing






