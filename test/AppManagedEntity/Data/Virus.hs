module AppManagedEntity.Data.Virus
  ( Virus(..)
  , VirusDiscoveredAt(..)
  , VirusId(..)
  , VirusName(..)
  , bpsDiscoveredAt
  , bpsVirusName
  , brnDiscoveredAt
  , brnVirusName
  , bpsVirus
  , brnVirus
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time as Time

data Virus = Virus
  { virusId :: VirusId
  , virusName :: VirusName
  , virusDiscoveredAt :: VirusDiscoveredAt
  } deriving (Show, Eq)

newtype VirusId = VirusId
  { unVirusId :: Int64
  } deriving (Show, Eq)

newtype VirusName = VirusName
  { unVirusName :: Text
  } deriving (Show, Eq)

newtype VirusDiscoveredAt = VirusDiscoveredAt
  { unVirusDiscoveredAt :: Time.UTCTime
  } deriving (Show, Eq)

bpsVirusName :: VirusName
bpsVirusName = VirusName (Text.pack "Bovine popular stomachitis")

brnVirusName :: VirusName
brnVirusName = VirusName (Text.pack "Black raspberry necrosis")

bpsDiscoveredAt :: VirusDiscoveredAt
bpsDiscoveredAt =
  VirusDiscoveredAt $ Time.UTCTime (Time.fromGregorian 1969 1 1) 0

brnDiscoveredAt :: VirusDiscoveredAt
brnDiscoveredAt =
  VirusDiscoveredAt $ Time.UTCTime (Time.fromGregorian 1800 12 25) 0

bpsVirus :: Virus
bpsVirus =
  Virus
    { virusId = VirusId 1
    , virusName = bpsVirusName
    , virusDiscoveredAt = bpsDiscoveredAt
    }

brnVirus :: Virus
brnVirus =
  Virus
    { virusId = VirusId 2
    , virusName = brnVirusName
    , virusDiscoveredAt = brnDiscoveredAt
    }
