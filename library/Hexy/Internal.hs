{-# LANGUAGE OverloadedStrings #-}

module Hexy.Internal where

import qualified Data.Char as Char
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable as Storable

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
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int8 -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int16 -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int32 -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Int64 -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word8 -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word16 -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word32 -> Builder #-}
{-# SPECIALIZE buildHex :: (Int -> Char) -> Word64 -> Builder #-}
buildHex = buildWordAtBase 16

buildWordAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> Builder
{-# SPECIALIZE buildWordAtBase :: Int -> (Int -> Char) -> Int -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Int8 -> (Int -> Char) -> Int8 -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Int16 -> (Int -> Char) -> Int16 -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Int32 -> (Int -> Char) -> Int32 -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Int64 -> (Int -> Char) -> Int64 -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Word -> (Int -> Char) -> Word -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Word8 -> (Int -> Char) -> Word8 -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Word16 -> (Int -> Char) -> Word16 -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Word32 -> (Int -> Char) -> Word32 -> Builder #-}
{-# SPECIALIZE buildWordAtBase :: Word64 -> (Int -> Char) -> Word64 -> Builder #-}
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
