module Test.PGGen
  ( pgText,
    pgDouble,
  )
where

import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

pgText :: HH.Range Int -> HH.Gen T.Text
pgText range =
  Gen.text range $
    Gen.filter (/= '\NUL') Gen.unicode

pgDouble :: HH.Gen Double
pgDouble =
  let -- Necessary because PostgreSQL only stores up to 15 digits of precision
      -- With a 3-digit range, this gives us 12 places after the decimal
      truncateLongDouble :: Double -> Double
      truncateLongDouble = (/ 1e12) . (fromIntegral :: Int -> Double) . round . (* 1e12)
   in flip Gen.subterm truncateLongDouble . Gen.double $ Range.linearFracFrom 0 (-1000) 1000
