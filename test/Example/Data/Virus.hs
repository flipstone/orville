module Example.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , VirusName(..)
  ) where

data Virus key = Virus
  { virusId :: key
  , virusName :: VirusName
  } deriving (Show, Eq)

newtype VirusId = VirusId
  { unVirusId :: Int
  } deriving (Show, Eq)

newtype VirusName = VirusName
  { unVirusName :: String
  } deriving (Show, Eq)
