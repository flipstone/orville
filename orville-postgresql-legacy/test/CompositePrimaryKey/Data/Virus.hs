module CompositePrimaryKey.Data.Virus
  ( Virus(..)
  , VirusType(..)
  , VirusSubType(..)
  , VirusName(..)
  , VirusKey
  , virusKey
  , bpsVirus
  , brnVirus
  ) where

import Data.Text (Text)
import qualified Data.Text as T

data Virus = Virus
  { virusType     :: VirusType
  , virusSubType  :: VirusSubType
  , virusName     :: VirusName
  } deriving (Show, Eq)

newtype VirusType = VirusType
  { unVirusType :: Text
  } deriving (Show, Eq, Ord)

newtype VirusSubType = VirusSubType
  { unVirusSubType :: Text
  } deriving (Show, Eq, Ord)

newtype VirusName = VirusName
  { unVirusName :: Text
  } deriving (Show, Eq)

type VirusKey = (VirusType, VirusSubType)

virusKey :: Virus -> VirusKey
virusKey virus =
  ( virusType virus
  , virusSubType virus
  )

bpsVirus :: Virus
bpsVirus =
  Virus
    { virusType     = VirusType (T.pack "stomachitis")
    , virusSubType  = VirusSubType (T.pack "bovine")
    , virusName     = VirusName (T.pack "Bovine popular stomachitis")
    }

brnVirus :: Virus
brnVirus =
  Virus
    { virusType     = VirusType (T.pack "necrosis")
    , virusSubType  = VirusSubType (T.pack "black-raspberry")
    , virusName     = VirusName (T.pack "Black raspberry necrosis")
    }

