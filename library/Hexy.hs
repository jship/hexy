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
xshow = Text.Lazy.toStrict . xshowl

-- | Converts a value to a zero-padded, prefixed, lowercase hexadecimal strict 'Text.Text'.
--
-- @
-- xshowp (0x1f :: Word32) = "0x0000001f"
-- @
xshowp :: HexShow a => a -> Text.Text
xshowp = Text.Lazy.toStrict . xshowlp

-- | Converts a value to a zero-padded, uppercase hexadecimal strict 'Text.Text'.
--
-- @
-- xshowu (0x1f :: Word32) = "0000001F"
-- @
xshowu :: HexShow a => a -> Text.Text
xshowu = Text.Lazy.toStrict . xshowlu

-- | Converts a value to a zero-padded, prefixed, uppercase hexadecimal strict 'Text.Text'.
--
-- @
-- xshowpu (0x1f :: Word32) = "0x0000001F"
-- @
xshowpu :: HexShow a => a -> Text.Text
xshowpu = Text.Lazy.toStrict . xshowlpu

-- | Converts a value to a zero-padded, lowercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowl (0x1f :: Word32) = "0000001f"
-- @
xshowl :: HexShow a => a -> Text.Lazy.Text
xshowl = Text.Lazy.Builder.toLazyText . xbuild

-- | Converts a value to a zero-padded, prefixed, lowercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowlp (0x1f :: Word32) = "0x0000001f"
-- @
xshowlp :: HexShow a => a -> Text.Lazy.Text
xshowlp = Text.Lazy.Builder.toLazyText . prefixHex . xbuild

-- | Converts a value to a zero-padded, uppercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowlu (0x1f :: Word32) = "0000001F"
-- @
xshowlu :: HexShow a => a -> Text.Lazy.Text
xshowlu = Text.Lazy.Builder.toLazyText . xbuildu

-- | Converts a value to a zero-padded, prefixed, uppercase hexadecimal lazy 'Text.Lazy.Text'.
--
-- @
-- xshowlpu (0x1f :: Word32) = "0x0000001F"
-- @
xshowlpu :: HexShow a => a -> Text.Lazy.Text
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
