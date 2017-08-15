module Hexy
  ( HexShow(..)
  ) where

import Hexy.Internal (showHexTextLower, showHexTextUpper, unsafeDropHexPrefix)

import Data.Int (Int, Int16, Int32, Int64, Int8)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Word (Word, Word8, Word16, Word32, Word64)

class HexShow a where
  xshow :: a -> Text.Text
  xshow = unsafeDropHexPrefix . xshowp

  xshowp :: a -> Text.Text

  xshowu :: a -> Text.Text
  xshowu = unsafeDropHexPrefix . xshowpu

  xshowpu :: a -> Text.Text

  xshowl :: a -> Text.Lazy.Text
  xshowl = Text.Lazy.fromStrict . xshow

  xshowlp :: a -> Text.Lazy.Text
  xshowlp = Text.Lazy.fromStrict . xshowp

  xshowlu :: a -> Text.Lazy.Text
  xshowlu = Text.Lazy.fromStrict . xshowu

  xshowlpu :: a -> Text.Lazy.Text
  xshowlpu = Text.Lazy.fromStrict . xshowpu

  {-# MINIMAL xshowp, xshowpu #-}

instance HexShow Int where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int8 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int16 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int32 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int64 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word8 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word16 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word32 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word64 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper
