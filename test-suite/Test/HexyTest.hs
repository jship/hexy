{-# LANGUAGE ScopedTypeVariables #-}

module Test.HexyTest where

import Hexy

import qualified Data.Char as Char
import Data.Int (Int, Int16, Int32, Int64, Int8)
import qualified Data.List as List
import Data.Monoid ((<>))
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable as Storable
import qualified Numeric as Numeric

import Test.QuickCheck.Instances ()
import Test.Tasty.QuickCheck (Positive(..))

data Casing = Casing'Lower | Casing'Upper

prop_xshow_Int :: Positive Int -> Bool
prop_xshow_Int (Positive v) = xshowEqShowHexStorable Casing'Lower v

prop_xshow_Int8 :: Positive Int8 -> Bool
prop_xshow_Int8 (Positive v) = xshowEqShowHexStorable Casing'Lower v

prop_xshow_Int16 :: Positive Int16 -> Bool
prop_xshow_Int16 (Positive v) = xshowEqShowHexStorable Casing'Lower v

prop_xshow_Int32 :: Positive Int32 -> Bool
prop_xshow_Int32 (Positive v) = xshowEqShowHexStorable Casing'Lower v

prop_xshow_Int64 :: Positive Int64 -> Bool
prop_xshow_Int64 (Positive v) = xshowEqShowHexStorable Casing'Lower v

prop_xshow_Word :: Word -> Bool
prop_xshow_Word = xshowEqShowHexStorable Casing'Lower

prop_xshow_Word8 :: Word8 -> Bool
prop_xshow_Word8 = xshowEqShowHexStorable Casing'Lower

prop_xshow_Word16 :: Word16 -> Bool
prop_xshow_Word16 = xshowEqShowHexStorable Casing'Lower

prop_xshow_Word32 :: Word32 -> Bool
prop_xshow_Word32 = xshowEqShowHexStorable Casing'Lower

prop_xshow_Word64 :: Word64 -> Bool
prop_xshow_Word64 = xshowEqShowHexStorable Casing'Lower

prop_xshowu_Int :: Positive Int -> Bool
prop_xshowu_Int (Positive v) = xshowEqShowHexStorable Casing'Upper v

prop_xshowu_Int8 :: Positive Int8 -> Bool
prop_xshowu_Int8 (Positive v) = xshowEqShowHexStorable Casing'Upper v

prop_xshowu_Int16 :: Positive Int16 -> Bool
prop_xshowu_Int16 (Positive v) = xshowEqShowHexStorable Casing'Upper v

prop_xshowu_Int32 :: Positive Int32 -> Bool
prop_xshowu_Int32 (Positive v) = xshowEqShowHexStorable Casing'Upper v

prop_xshowu_Int64 :: Positive Int64 -> Bool
prop_xshowu_Int64 (Positive v) = xshowEqShowHexStorable Casing'Upper v

prop_xshowu_Word :: Word -> Bool
prop_xshowu_Word = xshowEqShowHexStorable Casing'Upper

prop_xshowu_Word8 :: Word8 -> Bool
prop_xshowu_Word8 = xshowEqShowHexStorable Casing'Upper

prop_xshowu_Word16 :: Word16 -> Bool
prop_xshowu_Word16 = xshowEqShowHexStorable Casing'Upper

prop_xshowu_Word32 :: Word32 -> Bool
prop_xshowu_Word32 = xshowEqShowHexStorable Casing'Upper

prop_xshowu_Word64 :: Word64 -> Bool
prop_xshowu_Word64 = xshowEqShowHexStorable Casing'Upper

prop_xshowp_Int :: Positive Int -> Bool
prop_xshowp_Int (Positive v) = xshowpEqShowHexStorable Casing'Lower v

prop_xshowp_Int8 :: Positive Int8 -> Bool
prop_xshowp_Int8 (Positive v) = xshowpEqShowHexStorable Casing'Lower v

prop_xshowp_Int16 :: Positive Int16 -> Bool
prop_xshowp_Int16 (Positive v) = xshowpEqShowHexStorable Casing'Lower v

prop_xshowp_Int32 :: Positive Int32 -> Bool
prop_xshowp_Int32 (Positive v) = xshowpEqShowHexStorable Casing'Lower v

prop_xshowp_Int64 :: Positive Int64 -> Bool
prop_xshowp_Int64 (Positive v) = xshowpEqShowHexStorable Casing'Lower v

prop_xshowp_Word :: Word -> Bool
prop_xshowp_Word = xshowpEqShowHexStorable Casing'Lower

prop_xshowp_Word8 :: Word8 -> Bool
prop_xshowp_Word8 = xshowpEqShowHexStorable Casing'Lower

prop_xshowp_Word16 :: Word16 -> Bool
prop_xshowp_Word16 = xshowpEqShowHexStorable Casing'Lower

prop_xshowp_Word32 :: Word32 -> Bool
prop_xshowp_Word32 = xshowpEqShowHexStorable Casing'Lower

prop_xshowp_Word64 :: Word64 -> Bool
prop_xshowp_Word64 = xshowpEqShowHexStorable Casing'Lower

prop_xshowpu_Int :: Positive Int -> Bool
prop_xshowpu_Int (Positive v) = xshowpEqShowHexStorable Casing'Upper v

prop_xshowpu_Int8 :: Positive Int8 -> Bool
prop_xshowpu_Int8 (Positive v) = xshowpEqShowHexStorable Casing'Upper v

prop_xshowpu_Int16 :: Positive Int16 -> Bool
prop_xshowpu_Int16 (Positive v) = xshowpEqShowHexStorable Casing'Upper v

prop_xshowpu_Int32 :: Positive Int32 -> Bool
prop_xshowpu_Int32 (Positive v) = xshowpEqShowHexStorable Casing'Upper v

prop_xshowpu_Int64 :: Positive Int64 -> Bool
prop_xshowpu_Int64 (Positive v) = xshowpEqShowHexStorable Casing'Upper v

prop_xshowpu_Word :: Word -> Bool
prop_xshowpu_Word = xshowpEqShowHexStorable Casing'Upper

prop_xshowpu_Word8 :: Word8 -> Bool
prop_xshowpu_Word8 = xshowpEqShowHexStorable Casing'Upper

prop_xshowpu_Word16 :: Word16 -> Bool
prop_xshowpu_Word16 = xshowpEqShowHexStorable Casing'Upper

prop_xshowpu_Word32 :: Word32 -> Bool
prop_xshowpu_Word32 = xshowpEqShowHexStorable Casing'Upper

prop_xshowpu_Word64 :: Word64 -> Bool
prop_xshowpu_Word64 = xshowpEqShowHexStorable Casing'Upper

xshowEqShowHexStorable :: (HexShow a, Integral a, Show a, Storable a) => Casing -> a -> Bool
xshowEqShowHexStorable c v = showHexy c v == paddedHexViaShowHex c v

xshowpEqShowHexStorable :: (HexShow a, Integral a, Show a, Storable a) => Casing -> a -> Bool
xshowpEqShowHexStorable c v = showHexyWithPrefix c v == "0x" <> paddedHexViaShowHex c v

-- Functions to get results from the Hexy API
showHexy :: HexShow a => Casing -> a -> String
showHexy Casing'Lower v = xshow v
showHexy Casing'Upper v = xshowu v

showHexyWithPrefix :: HexShow a => Casing -> a -> String
showHexyWithPrefix Casing'Lower v = xshowp v
showHexyWithPrefix Casing'Upper v = xshowpu v

-- Functions to get results from base Numeric module
paddedHexViaShowHex :: forall a. (Integral a, Show a, Storable a) => Casing -> a -> String
paddedHexViaShowHex c v = (List.genericReplicate numPadCharsNeeded '0') <> hexViaNumeric
 where
  numPadCharsNeeded = lengthWithoutPrefix - List.genericLength hexViaNumeric
  lengthWithoutPrefix = fromIntegral (2 * Storable.sizeOf v) :: a
  hexViaNumeric = casedShowHex c v

casedShowHex :: (Integral a, Show a) => Casing -> a -> String
casedShowHex Casing'Lower v = Numeric.showHex v ""
casedShowHex Casing'Upper v = fmap Char.toUpper (Numeric.showHex v "")
