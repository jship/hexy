{-# LANGUAGE OverloadedStrings #-}

module Hexy.Internal where

import qualified Data.Char as Char
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable as Storable

xbuildStorable :: (Integral a, Show a, Storable a) => a -> Builder
xbuildStorable v = zeroPaddedHex . buildHex Char.intToDigit $ v
 where
  zeroPaddedHex hex = go numPadCharsNeeded hex
   where
    go 0 b = b
    go n b = go (n - 1) (Text.Lazy.Builder.singleton '0' <> b)
    numPadCharsNeeded = lengthWithoutPrefix - hexLength
    lengthWithoutPrefix = fromIntegral $ 2 * Storable.sizeOf v
    hexLength = Text.Lazy.length . Text.Lazy.Builder.toLazyText $ hex

xbuilduStorable :: (Integral a, Show a, Storable a) => a -> Builder
xbuilduStorable v = zeroPaddedHex . buildHex (Char.toUpper . Char.intToDigit) $ v
 where
  zeroPaddedHex hex = go numPadCharsNeeded hex
   where
    go 0 b = b
    go n b = go (n - 1) (Text.Lazy.Builder.singleton '0' <> b)
    numPadCharsNeeded = lengthWithoutPrefix - hexLength
    lengthWithoutPrefix = fromIntegral $ 2 * Storable.sizeOf v
    hexLength = Text.Lazy.length . Text.Lazy.Builder.toLazyText $ hex

prefixHex :: Builder -> Builder
prefixHex b = "0x" <> b

buildHex :: (Integral a, Show a) => (Int -> Char) -> a -> Builder
buildHex = buildWordAtBase 16

buildWordAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> Builder
buildWordAtBase base toChr n0
  | base <= 1 = errorWithoutStackTrace ("Hexy.buildWordAtBase: applied to unsupported base " ++ show base)
  | n0   <  0 = errorWithoutStackTrace ("Hexy.buildWordAtBase: applied to negative number " ++ show n0)
  | otherwise = buildIt (quotRem n0 base) mempty
     where
      buildIt (n, d) b = seq c $ -- stricter than necessary
        case n of
          0 -> b'
          _ -> buildIt (quotRem n base) b'
       where
        c = toChr $ fromIntegral d
        b' = Text.Lazy.Builder.singleton c <> b
