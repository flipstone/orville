module EntityWrapper.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , VirusName(..)
  , bpsVirus
  , brnVirus
  ) where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text

data Virus = Virus
  { virusName :: VirusName
  } deriving (Show, Eq)

newtype VirusId = VirusId
  { unVirusId :: Int32
  } deriving (Show, Eq)

newtype VirusName = VirusName
  { unVirusName :: Text
  } deriving (Show, Eq)

bpsVirus :: Virus
bpsVirus =
  Virus {virusName = VirusName (Text.pack "Bovine popular stomachitis")}

brnVirus :: Virus
brnVirus = Virus {virusName = VirusName (Text.pack "Black raspberry necrosis")}
