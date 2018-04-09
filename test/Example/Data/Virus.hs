module Example.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , VirusName(..)
  ) where

import Data.Text (Text)

data Virus key = Virus
  { virusId :: key
  , virusName :: VirusName
  } deriving (Show, Eq)

newtype VirusId = VirusId
  { unVirusId :: Int
  } deriving (Show, Eq)

newtype VirusName = VirusName
  { unVirusName :: Text
  } deriving (Show, Eq)
