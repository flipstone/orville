module AppManagedEntity.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , VirusName(..)
  , bpsVirusName
  , brnVirusName
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text

data Virus = Virus
  { virusId :: VirusId
  , virusName :: VirusName
  } deriving (Show, Eq)

newtype VirusId = VirusId
  { unVirusId :: Int64
  } deriving (Show, Eq)

newtype VirusName = VirusName
  { unVirusName :: Text
  } deriving (Show, Eq)

bpsVirusName :: VirusName
bpsVirusName = VirusName (Text.pack "Bovine popular stomachitis")

brnVirusName :: VirusName
brnVirusName = VirusName (Text.pack "Black raspberry necrosis")
