module Hexy
  ( HexShow(..)
  , xshowp
  , xshowpu
  ) where

import Hexy.Internal

import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)

-- | Conversion of values to hexadecimal 'String's.
class HexShow a where
  -- | Builds a zero-padded, lowercase hexadecimal `String` from a value.
  --
  -- @
  -- xshow (0x1f :: Word32) = "0000001f"
  -- @
  xshow :: a -> String

  -- | Builds a zero-padded, uppercase hexadecimal `String` from a value.
  --
  -- @
  -- xshowu (0x1f :: Word32) = "0000001F"
  -- @
  xshowu :: a -> String

-- | Converts a value to a zero-padded, prefixed, lowercase hexadecimal `String`.
--
-- @
-- xshowp (0x1f :: Word32) = "0x0000001f"
-- @
xshowp :: HexShow a => a -> String
xshowp = prefixHex . xshow

-- | Converts a value to a zero-padded, prefixed, uppercase hexadecimal `String`.
--
-- @
-- xshowpu (0x1f :: Word32) = "0x0000001F"
-- @
xshowpu :: HexShow a => a -> String
xshowpu = prefixHex . xshowu

instance HexShow Int where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Int8 where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Int16 where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Int32 where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Int64 where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Word where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Word8 where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Word16 where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Word32 where
  xshow = xshowStorable
  xshowu = xshowuStorable

instance HexShow Word64 where
  xshow = xshowStorable
  xshowu = xshowuStorable
