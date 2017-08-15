{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.HexyTest where

import Hexy

import qualified Data.Char as Char
import Data.Int (Int, Int16, Int32, Int64, Int8)
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable as Storable
import qualified Numeric as Numeric

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Positive(..))

data Casing    = Casing'Lower    | Casing'Upper
data Prefixing = Prefixing'No    | Prefixing'Yes
data TextType  = TextType'Strict | TextType'Lazy

prop_xshow_Int :: Positive Int -> Bool
prop_xshow_Int (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Int :: Positive Int -> Bool
prop_xshowu_Int (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Int :: Positive Int -> Bool
prop_xshowp_Int (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Int :: Positive Int -> Bool
prop_xshowpu_Int (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Int :: Positive Int -> Bool
prop_xshowl_Int (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Int :: Positive Int -> Bool
prop_xshowlu_Int (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Int :: Positive Int -> Bool
prop_xshowlp_Int (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Int :: Positive Int -> Bool
prop_xshowlpu_Int (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Int8 :: Positive Int8 -> Bool
prop_xshow_Int8 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Int8 :: Positive Int8 -> Bool
prop_xshowu_Int8 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Int8 :: Positive Int8 -> Bool
prop_xshowp_Int8 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Int8 :: Positive Int8 -> Bool
prop_xshowpu_Int8 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Int8 :: Positive Int8 -> Bool
prop_xshowl_Int8 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Int8 :: Positive Int8 -> Bool
prop_xshowlu_Int8 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Int8 :: Positive Int8 -> Bool
prop_xshowlp_Int8 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Int8 :: Positive Int8 -> Bool
prop_xshowlpu_Int8 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Int16 :: Positive Int16 -> Bool
prop_xshow_Int16 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Int16 :: Positive Int16 -> Bool
prop_xshowu_Int16 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Int16 :: Positive Int16 -> Bool
prop_xshowp_Int16 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Int16 :: Positive Int16 -> Bool
prop_xshowpu_Int16 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Int16 :: Positive Int16 -> Bool
prop_xshowl_Int16 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Int16 :: Positive Int16 -> Bool
prop_xshowlu_Int16 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Int16 :: Positive Int16 -> Bool
prop_xshowlp_Int16 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Int16 :: Positive Int16 -> Bool
prop_xshowlpu_Int16 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Int32 :: Positive Int32 -> Bool
prop_xshow_Int32 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Int32 :: Positive Int32 -> Bool
prop_xshowu_Int32 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Int32 :: Positive Int32 -> Bool
prop_xshowp_Int32 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Int32 :: Positive Int32 -> Bool
prop_xshowpu_Int32 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Int32 :: Positive Int32 -> Bool
prop_xshowl_Int32 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Int32 :: Positive Int32 -> Bool
prop_xshowlu_Int32 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Int32 :: Positive Int32 -> Bool
prop_xshowlp_Int32 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Int32 :: Positive Int32 -> Bool
prop_xshowlpu_Int32 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Int64 :: Positive Int64 -> Bool
prop_xshow_Int64 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Int64 :: Positive Int64 -> Bool
prop_xshowu_Int64 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Int64 :: Positive Int64 -> Bool
prop_xshowp_Int64 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Int64 :: Positive Int64 -> Bool
prop_xshowpu_Int64 (Positive v) = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Int64 :: Positive Int64 -> Bool
prop_xshowl_Int64 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Int64 :: Positive Int64 -> Bool
prop_xshowlu_Int64 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Int64 :: Positive Int64 -> Bool
prop_xshowlp_Int64 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Int64 :: Positive Int64 -> Bool
prop_xshowlpu_Int64 (Positive v) = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Word :: Word -> Bool
prop_xshow_Word v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Word :: Word -> Bool
prop_xshowu_Word v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Word :: Word -> Bool
prop_xshowp_Word v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Word :: Word -> Bool
prop_xshowpu_Word v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Word :: Word -> Bool
prop_xshowl_Word v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Word :: Word -> Bool
prop_xshowlu_Word v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Word :: Word -> Bool
prop_xshowlp_Word v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Word :: Word -> Bool
prop_xshowlpu_Word v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Word8 :: Word8 -> Bool
prop_xshow_Word8 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Word8 :: Word8 -> Bool
prop_xshowu_Word8 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Word8 :: Word8 -> Bool
prop_xshowp_Word8 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Word8 :: Word8 -> Bool
prop_xshowpu_Word8 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Word8 :: Word8 -> Bool
prop_xshowl_Word8 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Word8 :: Word8 -> Bool
prop_xshowlu_Word8 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Word8 :: Word8 -> Bool
prop_xshowlp_Word8 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Word8 :: Word8 -> Bool
prop_xshowlpu_Word8 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Word16 :: Word16 -> Bool
prop_xshow_Word16 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Word16 :: Word16 -> Bool
prop_xshowu_Word16 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Word16 :: Word16 -> Bool
prop_xshowp_Word16 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Word16 :: Word16 -> Bool
prop_xshowpu_Word16 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Word16 :: Word16 -> Bool
prop_xshowl_Word16 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Word16 :: Word16 -> Bool
prop_xshowlu_Word16 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Word16 :: Word16 -> Bool
prop_xshowlp_Word16 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Word16 :: Word16 -> Bool
prop_xshowlpu_Word16 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Word32 :: Word32 -> Bool
prop_xshow_Word32 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Word32 :: Word32 -> Bool
prop_xshowu_Word32 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Word32 :: Word32 -> Bool
prop_xshowp_Word32 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Word32 :: Word32 -> Bool
prop_xshowpu_Word32 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Word32 :: Word32 -> Bool
prop_xshowl_Word32 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Word32 :: Word32 -> Bool
prop_xshowlu_Word32 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Word32 :: Word32 -> Bool
prop_xshowlp_Word32 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Word32 :: Word32 -> Bool
prop_xshowlpu_Word32 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

prop_xshow_Word64 :: Word64 -> Bool
prop_xshow_Word64 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Lower v

prop_xshowu_Word64 :: Word64 -> Bool
prop_xshowu_Word64 v = xshowEqShowHexStorable TextType'Strict Prefixing'No Casing'Upper v

prop_xshowp_Word64 :: Word64 -> Bool
prop_xshowp_Word64 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Lower v

prop_xshowpu_Word64 :: Word64 -> Bool
prop_xshowpu_Word64 v = xshowEqShowHexStorable TextType'Strict Prefixing'Yes Casing'Upper v

prop_xshowl_Word64 :: Word64 -> Bool
prop_xshowl_Word64 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Lower v

prop_xshowlu_Word64 :: Word64 -> Bool
prop_xshowlu_Word64 v = xshowEqShowHexStorable TextType'Lazy Prefixing'No Casing'Upper v

prop_xshowlp_Word64 :: Word64 -> Bool
prop_xshowlp_Word64 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Lower v

prop_xshowlpu_Word64 :: Word64 -> Bool
prop_xshowlpu_Word64 v = xshowEqShowHexStorable TextType'Lazy Prefixing'Yes Casing'Upper v

xshowEqShowHexStorable :: (HexShow a, Integral a, Show a, Storable a) => TextType -> Prefixing -> Casing -> a -> Bool
xshowEqShowHexStorable t p c v = showHexy t p c v == paddedHexViaShowHex p c v

-- Functions to get results from the Hexy API
showHexy :: HexShow a => TextType -> Prefixing -> Casing -> a -> Text.Text
showHexy TextType'Strict Prefixing'No  Casing'Lower = xshow
showHexy TextType'Strict Prefixing'No  Casing'Upper = xshowu
showHexy TextType'Strict Prefixing'Yes Casing'Lower = xshowp
showHexy TextType'Strict Prefixing'Yes Casing'Upper = xshowpu
showHexy TextType'Lazy   Prefixing'No  Casing'Lower = Text.Lazy.toStrict . xshowl
showHexy TextType'Lazy   Prefixing'No  Casing'Upper = Text.Lazy.toStrict . xshowlu
showHexy TextType'Lazy   Prefixing'Yes Casing'Lower = Text.Lazy.toStrict . xshowlp
showHexy TextType'Lazy   Prefixing'Yes Casing'Upper = Text.Lazy.toStrict . xshowlpu

-- Functions to get results from base Numeric module
paddedHexViaShowHex :: forall a. (Integral a, Show a, Storable a) => Prefixing -> Casing -> a -> Text.Text
paddedHexViaShowHex p c v = case p of
  Prefixing'No  -> hexWithoutPrefix
  Prefixing'Yes -> "0x" <> hexWithoutPrefix
 where
  hexWithoutPrefix = Text.pack ((List.genericReplicate numPadCharsNeeded '0') <> hexViaNumeric)
  numPadCharsNeeded = lengthWithoutPrefix - List.genericLength hexViaNumeric
  lengthWithoutPrefix = fromIntegral (2 * Storable.sizeOf v) :: a
  hexViaNumeric = casedShowHex c v

casedShowHex :: (Integral a, Show a) => Casing -> a -> String
casedShowHex Casing'Lower v = Numeric.showHex v ""
casedShowHex Casing'Upper v = fmap Char.toUpper (Numeric.showHex v "")
