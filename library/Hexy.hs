module Hexy
  ( HexShow(..)
  , xshow
  , xshowp
  , xshowu
  , xshowpu
  , xshowl
  , xshowlp
  , xshowlu
  , xshowlpu
  ) where

import Hexy.Internal

import Data.Int (Int, Int16, Int32, Int64, Int8)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import Data.Word (Word, Word16, Word32, Word64, Word8)

-- | Conversion of values to hexadecimal strings.
class HexShow a where
  -- | Creates a zero-padded, lowercase hexadecimal 'Builder' from a value.
  --
  -- @
  -- xbuild (0x1f :: Word32) = "0000001f"
  -- @
  xbuild :: a -> Builder

  -- | Creates a zero-padded, uppercase hexadecimal 'Builder' from a value.
  --
  -- @
  -- xbuildu (0x1f :: Word32) = "0000001F"
  -- @
  xbuildu :: a -> Builder

-- | Converts a value to a zero-padded, lowercase hexadecimal strict 'Text.Text'.
--
-- @
-- xshow (0x1f :: Word32) = "0000001f"
-- @
xshow :: HexShow a => a -> Text.Text
{-# SPECIALIZE xshow :: Int -> Text.Text #-}
{-# SPECIALIZE xshow :: Int8 -> Text.Text #-}
{-# SPECIALIZE xshow :: Int16 -> Text.Text #-}
{-# SPECIALIZE xshow :: Int32 -> Text.Text #-}
{-# SPECIALIZE xshow :: Int64 -> Text.Text #-}
{-# SPECIALIZE xshow :: Word -> Text.Text #-}
{-# SPECIALIZE xshow :: Word8 -> Text.Text #-}
{-# SPECIALIZE xshow :: Word16 -> Text.Text #-}
{-# SPECIALIZE xshow :: Word32 -> Text.Text #-}
{-# SPECIALIZE xshow :: Word64 -> Text.Text #-}
xshow = Text.Lazy.toStrict . xshowl

-- | Converts a value to a zero-padded, prefixed, lowercase hexadecimal strict 'Text.Text'.
--
-- @
-- xshowp (0x1f :: Word32) = "0x0000001f"
-- @
xshowp :: HexShow a => a -> Text.Text
{-# SPECIALIZE xshowp :: Int -> Text.Text #-}
{-# SPECIALIZE xshowp :: Int8 -> Text.Text #-}
{-# SPECIALIZE xshowp :: Int16 -> Text.Text #-}
{-# SPECIALIZE xshowp :: Int32 -> Text.Text #-}
{-# SPECIALIZE xshowp :: Int64 -> Text.Text #-}
{-# SPECIALIZE xshowp :: Word -> Text.Text #-}
{-# SPECIALIZE xshowp :: Word8 -> Text.Text #-}
{-# SPECIALIZE xshowp :: Word16 -> Text.Text #-}
{-# SPECIALIZE xshowp :: Word32 -> Text.Text #-}
{-# SPECIALIZE xshowp :: Word64 -> Text.Text #-}
xshowp = Text.Lazy.toStrict . xshowlp

-- | Converts a value to a zero-padded, uppercase hexadecimal strict 'Text.Text'.
--
-- @
-- xshowu (0x1f :: Word32) = "0000001F"
-- @
xshowu :: HexShow a => a -> Text.Text
{-# SPECIALIZE xshowu :: Int -> Text.Text #-}
{-# SPECIALIZE xshowu :: Int8 -> Text.Text #-}
{-# SPECIALIZE xshowu :: Int16 -> Text.Text #-}
{-# SPECIALIZE xshowu :: Int32 -> Text.Text #-}
{-# SPECIALIZE xshowu :: Int64 -> Text.Text #-}
{-# SPECIALIZE xshowu :: Word -> Text.Text #-}
{-# SPECIALIZE xshowu :: Word8 -> Text.Text #-}
{-# SPECIALIZE xshowu :: Word16 -> Text.Text #-}
{-# SPECIALIZE xshowu :: Word32 -> Text.Text #-}
{-# SPECIALIZE xshowu :: Word64 -> Text.Text #-}
xshowu = Text.Lazy.toStrict . xshowlu

-- | Converts a value to a zero-padded, prefixed, uppercase hexadecimal strict 'Text.Text'.
--
-- @
-- xshowpu (0x1f :: Word32) = "0x0000001F"
-- @
xshowpu :: HexShow a => a -> Text.Text
{-# SPECIALIZE xshowpu :: Int -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Int8 -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Int16 -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Int32 -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Int64 -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Word -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Word8 -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Word16 -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Word32 -> Text.Text #-}
{-# SPECIALIZE xshowpu :: Word64 -> Text.Text #-}
xshowpu = Text.Lazy.toStrict . xshowlpu

-- | Converts a value to a zero-padded, lowercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowl (0x1f :: Word32) = "0000001f"
-- @
xshowl :: HexShow a => a -> Text.Lazy.Text
{-# SPECIALIZE xshowl :: Int -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Int8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Int16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Int32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Int64 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Word -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Word8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Word16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Word32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowl :: Word64 -> Text.Lazy.Text #-}
xshowl = Text.Lazy.Builder.toLazyText . xbuild

-- | Converts a value to a zero-padded, prefixed, lowercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowlp (0x1f :: Word32) = "0x0000001f"
-- @
xshowlp :: HexShow a => a -> Text.Lazy.Text
{-# SPECIALIZE xshowlp :: Int -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Int8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Int16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Int32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Int64 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Word -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Word8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Word16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Word32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlp :: Word64 -> Text.Lazy.Text #-}
xshowlp = Text.Lazy.Builder.toLazyText . prefixHex . xbuild

-- | Converts a value to a zero-padded, uppercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowlu (0x1f :: Word32) = "0000001F"
-- @
xshowlu :: HexShow a => a -> Text.Lazy.Text
{-# SPECIALIZE xshowlu :: Int -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Int8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Int16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Int32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Int64 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Word -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Word8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Word16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Word32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlu :: Word64 -> Text.Lazy.Text #-}
xshowlu = Text.Lazy.Builder.toLazyText . xbuildu

-- | Converts a value to a zero-padded, prefixed, uppercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowlpu (0x1f :: Word32) = "0x0000001F"
-- @
xshowlpu :: HexShow a => a -> Text.Lazy.Text
{-# SPECIALIZE xshowlpu :: Int -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Int8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Int16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Int32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Int64 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Word -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Word8 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Word16 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Word32 -> Text.Lazy.Text #-}
{-# SPECIALIZE xshowlpu :: Word64 -> Text.Lazy.Text #-}
xshowlpu = Text.Lazy.Builder.toLazyText . prefixHex . xbuildu

instance HexShow Int where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Int8 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Int16 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Int32 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Int64 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Word where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Word8 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Word16 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Word32 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable

instance HexShow Word64 where
  xbuild = xbuildStorable
  xbuildu = xbuilduStorable
