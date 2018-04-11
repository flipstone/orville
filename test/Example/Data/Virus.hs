module Example.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , VirusName(..)
  , newVirus
  , newVirusId
  ) where

import Control.Applicative (liftA2)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 as UUIDv4

data Virus = Virus
  { virusId :: VirusId
  , virusName :: VirusName
  } deriving (Show, Eq)

newVirus :: VirusName -> IO Virus
newVirus name = liftA2 Virus newVirusId (pure name)

newtype VirusId = VirusId
  { unVirusId :: UUID
  } deriving (Show, Eq)

-- This uses V4 UUIDS because they can be reliably generated.
-- Applications should *probably* be using V1 UUIDs. We should
-- determine how best to provide a convenient helper for generating
-- those as the UUID library returns a Nothing if you generate
-- them too quickly.
newVirusId :: IO VirusId
newVirusId = fmap VirusId UUIDv4.nextRandom

newtype VirusName = VirusName
  { unVirusName :: Text
  } deriving (Show, Eq)
