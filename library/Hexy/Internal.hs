module Hexy.Internal
  ( xshowStorable
  , xshowuStorable
  , prefixHex
  ) where

import qualified Data.Char as Char
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import qualified Numeric

xshowStorable :: (Integral a, Show a, Storable a) => a -> String
xshowStorable v = zeroPaddedHex (Numeric.showHex v "")
 where
  zeroPaddedHex hex = go numPadCharsNeeded hex
   where
    go 0 s = s
    go n s = go (n - 1) ('0' : s)
    numPadCharsNeeded = lengthWithoutPrefix - length hex
    lengthWithoutPrefix = fromIntegral $ 2 * Storable.sizeOf v

xshowuStorable :: (Integral a, Show a, Storable a) => a -> String
xshowuStorable = fmap Char.toUpper . xshowStorable

prefixHex :: String -> String
prefixHex hex = '0' : 'x' : hex
