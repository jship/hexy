{-# LANGUAGE BangPatterns #-}

module Hexy.Internal where

import Control.Monad.ST (ST)
import qualified Data.Char as Char
import Data.Int (Int, Int8, Int16, Int32, Int64)
import qualified Data.Text as Text
import qualified Data.Text.Array as Text.Array
import qualified Data.Text.Internal as Text.Internal
import qualified Data.Text.Internal.Private as Text.Internal.Private
import qualified Data.Text.Internal.Unsafe.Char as Text.Internal.Unsafe.Char
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable as Storable

showHexTextLower :: (Integral a, Show a, Storable a) => a -> Text.Text
{-# SPECIALIZE showHexTextLower :: Int -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Int8 -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Int16 -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Int32 -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Int64 -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Word -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Word8 -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Word16 -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Word32 -> Text.Text #-}
{-# SPECIALIZE showHexTextLower :: Word64 -> Text.Text #-}
showHexTextLower = textShowIntAtBase 16 intToDigitLower

showHexTextUpper :: (Integral a, Show a, Storable a) => a -> Text.Text
{-# SPECIALIZE showHexTextUpper :: Int -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Int8 -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Int16 -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Int32 -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Int64 -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Word -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Word8 -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Word16 -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Word32 -> Text.Text #-}
{-# SPECIALIZE showHexTextUpper :: Word64 -> Text.Text #-}
showHexTextUpper = textShowIntAtBase 16 intToDigitUpper

textShowIntAtBase :: (Integral a, Show a, Storable a) => a -> (Int -> Char) -> a -> Text.Text
{-# SPECIALIZE textShowIntAtBase :: Int -> (Int -> Char) -> Int -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Int8 -> (Int -> Char) -> Int8 -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Int16 -> (Int -> Char) -> Int16 -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Int32 -> (Int -> Char) -> Int32 -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Int64 -> (Int -> Char) -> Int64 -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Word -> (Int -> Char) -> Word -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Word8 -> (Int -> Char) -> Word8 -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Word16 -> (Int -> Char) -> Word16 -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Word32 -> (Int -> Char) -> Word32 -> Text.Text #-}
{-# SPECIALIZE textShowIntAtBase :: Word64 -> (Int -> Char) -> Word64 -> Text.Text #-}
textShowIntAtBase base toChr n0
  | base <= 1 = errorWithoutStackTrace ("Hexy.Internal.textShowIntAtBase: applied to unsupported base " ++ show base)
  | n0   <  0 = errorWithoutStackTrace ("Hexy.Internal.textShowIntAtBase: applied to negative number " ++ show n0)
  | otherwise = Text.Internal.Private.runText $ \done -> do
      let !size = 2 + (2 * Storable.sizeOf n0)
      buffer <- Text.Array.new size
      let hexLoop i (n, d) = do
            i' <- unsafeWriteRev buffer i c
            case n of
              0 -> pure i'
              _ -> hexLoop i' (quotRem n base)
           where
            c = toChr $ fromIntegral d
      let zeroPadLoop i
            | i < 2 = pure i
            | otherwise = do
                i' <- unsafeWriteRev buffer i '0'
                zeroPadLoop i'
      j <- hexLoop (size - 1) (quotRem n0 base)
      k <- zeroPadLoop j
      l <- unsafeWriteRev buffer k 'x'
      _ <- unsafeWriteRev buffer l '0'
      done buffer size

unsafeWriteRev :: Text.Array.MArray s -> Int -> Char -> ST s Int
unsafeWriteRev buffer i c = do
  Text.Array.unsafeWrite buffer i (fromIntegral (Text.Internal.Unsafe.Char.ord c))
  pure (i - 1)

unsafeDropHexPrefix :: Text.Text -> Text.Text
unsafeDropHexPrefix (Text.Internal.Text arr _ len) = Text.Internal.Text arr 2 (len - 2)

intToDigitLower :: Int -> Char
intToDigitLower i
  | i >=  0 && i <=  9 = Text.Internal.Unsafe.Char.unsafeChr (fromIntegral $ Char.ord '0' + i)
  | i >= 10 && i <= 15 = Text.Internal.Unsafe.Char.unsafeChr (fromIntegral $ Char.ord 'a' + i - 10)
  | otherwise = errorWithoutStackTrace ("Hexy.Internal.intToDigitLower: not a digit " ++ show i)

intToDigitUpper :: Int -> Char
intToDigitUpper i
  | i >=  0 && i <=  9 = Text.Internal.Unsafe.Char.unsafeChr (fromIntegral $ Char.ord '0' + i)
  | i >= 10 && i <= 15 = Text.Internal.Unsafe.Char.unsafeChr (fromIntegral $ Char.ord 'A' + i - 10)
  | otherwise = errorWithoutStackTrace ("Hexy.Internal.intToDigitUpper: not a digit " ++ show i)
