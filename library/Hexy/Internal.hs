{-# LANGUAGE OverloadedStrings #-}

module Hexy.Internal where

import qualified Data.Char as Char
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable as Storable

data LengthTrackingBuilder a = LengthTrackingBuilder !a Builder

xbuildStorable :: (Integral a, Show a, Storable a) => a -> Builder
{-# SPECIALIZE xbuildStorable :: Int -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Int8 -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Int16 -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Int32 -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Int64 -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Word -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Word8 -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Word16 -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Word32 -> Builder #-}
{-# SPECIALIZE xbuildStorable :: Word64 -> Builder #-}
xbuildStorable = zeroPaddedHex . buildHex Char.intToDigit

xbuilduStorable :: (Integral a, Show a, Storable a) => a -> Builder
{-# SPECIALIZE xbuilduStorable :: Int -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Int8 -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Int16 -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Int32 -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Int64 -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Word -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Word8 -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Word16 -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Word32 -> Builder #-}
{-# SPECIALIZE xbuilduStorable :: Word64 -> Builder #-}
xbuilduStorable = zeroPaddedHex . buildHex (Char.toUpper . Char.intToDigit)

zeroPaddedHex :: (Integral a, Storable a) => LengthTrackingBuilder a -> Builder
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Int -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Int8 -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Int16 -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Int32 -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Int64 -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Word -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Word8 -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Word16 -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Word32 -> Builder #-}
{-# SPECIALIZE zeroPaddedHex :: LengthTrackingBuilder Word64 -> Builder #-}
zeroPaddedHex (LengthTrackingBuilder len hex) = go numPadCharsNeeded hex
 where
  go 0 b = b
  go n b = go (n - 1) (Text.Lazy.Builder.singleton '0' <> b)
  numPadCharsNeeded = lengthWithoutPrefix - len
  lengthWithoutPrefix = fromIntegral $ 2 * Storable.sizeOf len

prefixHex :: Builder -> Builder
prefixHex b = "0x" <> b

buildHex :: (Integral a, Show a) => (Int -> Char) -> a -> LengthTrackingBuilder a
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int -> LengthTrackingBuilder Int #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int8 -> LengthTrackingBuilder Int8 #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int16 -> LengthTrackingBuilder Int16 #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int32 -> LengthTrackingBuilder Int32 #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int64 -> LengthTrackingBuilder Int64 #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word -> LengthTrackingBuilder Word #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word8 -> LengthTrackingBuilder Word8 #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word16 -> LengthTrackingBuilder Word16 #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word32 -> LengthTrackingBuilder Word32 #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word64 -> LengthTrackingBuilder Word64 #-}
buildHex = buildWordAtBase 16

buildWordAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> LengthTrackingBuilder a
{-# SPECIALIZE buildWordAtBase :: Int -> (Int -> Char) -> Int -> LengthTrackingBuilder Int #-}
{-# SPECIALIZE buildWordAtBase :: Int8 -> (Int -> Char) -> Int8 -> LengthTrackingBuilder Int8 #-}
{-# SPECIALIZE buildWordAtBase :: Int16 -> (Int -> Char) -> Int16 -> LengthTrackingBuilder Int16 #-}
{-# SPECIALIZE buildWordAtBase :: Int32 -> (Int -> Char) -> Int32 -> LengthTrackingBuilder Int32 #-}
{-# SPECIALIZE buildWordAtBase :: Int64 -> (Int -> Char) -> Int64 -> LengthTrackingBuilder Int64 #-}
{-# SPECIALIZE buildWordAtBase :: Word -> (Int -> Char) -> Word -> LengthTrackingBuilder Word #-}
{-# SPECIALIZE buildWordAtBase :: Word8 -> (Int -> Char) -> Word8 -> LengthTrackingBuilder Word8 #-}
{-# SPECIALIZE buildWordAtBase :: Word16 -> (Int -> Char) -> Word16 -> LengthTrackingBuilder Word16 #-}
{-# SPECIALIZE buildWordAtBase :: Word32 -> (Int -> Char) -> Word32 -> LengthTrackingBuilder Word32 #-}
{-# SPECIALIZE buildWordAtBase :: Word64 -> (Int -> Char) -> Word64 -> LengthTrackingBuilder Word64 #-}
buildWordAtBase base toChr n0
  | base <= 1 = errorWithoutStackTrace ("Hexy.buildWordAtBase: applied to unsupported base " ++ show base)
  | n0   <  0 = errorWithoutStackTrace ("Hexy.buildWordAtBase: applied to negative number " ++ show n0)
  | otherwise = buildIt (quotRem n0 base) (LengthTrackingBuilder 1 mempty)
     where
      buildIt (n, d) (LengthTrackingBuilder l b) = seq c $ -- stricter than necessary
        case n of
          0 -> LengthTrackingBuilder l b'
          _ -> buildIt (quotRem n base) (LengthTrackingBuilder (1 + l) b')
       where
        c = toChr $ fromIntegral d
        b' = Text.Lazy.Builder.singleton c <> b
