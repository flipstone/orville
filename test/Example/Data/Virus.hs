module Example.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , VirusName(..)
  , bpsVirus
  , brnVirus
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

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

bpsVirus :: Virus ()
bpsVirus =
  Virus
    { virusId = ()
    , virusName = VirusName (Text.pack "Bovine popular stomachitis")
    }

brnVirus :: Virus ()
brnVirus =
  Virus
    { virusId = ()
    , virusName = VirusName (Text.pack "Black raspberry necrosis")
    }
