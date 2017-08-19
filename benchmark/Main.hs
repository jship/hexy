import Hexy

import Criterion.Main
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Numeric (showHex)
import Text.Printf (printf)

main :: IO ()
main = defaultMain
  [ bgroup "Int"
    [ bench "printf"   $ nf (printf "%08x" :: Int -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Int)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Int)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Int)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Int)
    , bench "xshow"    $ nf xshow    (0x1f :: Int)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Int)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Int)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Int)
    ]
  , bgroup "Int8"
    [ bench "printf"   $ nf (printf "%08x" :: Int8 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int8)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Int8)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Int8)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Int8)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Int8)
    , bench "xshow"    $ nf xshow    (0x1f :: Int8)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Int8)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Int8)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Int8)
    ]
  , bgroup "Int16"
    [ bench "printf"   $ nf (printf "%08x" :: Int16 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int16)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Int16)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Int16)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Int16)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Int16)
    , bench "xshow"    $ nf xshow    (0x1f :: Int16)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Int16)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Int16)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Int16)
    ]
  , bgroup "Int32"
    [ bench "printf"   $ nf (printf "%08x" :: Int32 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int32)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Int32)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Int32)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Int32)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Int32)
    , bench "xshow"    $ nf xshow    (0x1f :: Int32)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Int32)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Int32)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Int32)
    ]
  , bgroup "Int64"
    [ bench "printf"   $ nf (printf "%08x" :: Int64 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int64)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Int64)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Int64)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Int64)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Int64)
    , bench "xshow"    $ nf xshow    (0x1f :: Int64)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Int64)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Int64)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Int64)
    ]
  , bgroup "Word"
    [ bench "printf"   $ nf (printf "%08x" :: Word -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Word)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Word)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Word)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Word)
    , bench "xshow"    $ nf xshow    (0x1f :: Word)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Word)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Word)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Word)
    ]
  , bgroup "Word8"
    [ bench "printf"   $ nf (printf "%08x" :: Word8 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word8)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Word8)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Word8)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Word8)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Word8)
    , bench "xshow"    $ nf xshow    (0x1f :: Word8)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Word8)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Word8)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Word8)
    ]
  , bgroup "Word16"
    [ bench "printf"   $ nf (printf "%08x" :: Word16 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word16)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Word16)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Word16)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Word16)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Word16)
    , bench "xshow"    $ nf xshow    (0x1f :: Word16)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Word16)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Word16)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Word16)
    ]
  , bgroup "Word32"
    [ bench "printf"   $ nf (printf "%08x" :: Word32 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word32)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Word32)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Word32)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Word32)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Word32)
    , bench "xshow"    $ nf xshow    (0x1f :: Word32)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Word32)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Word32)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Word32)
    ]
  , bgroup "Word64"
    [ bench "printf"   $ nf (printf "%08x" :: Word64 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word64)) ""
    , bench "xshowl"   $ nf xshowl   (0x1f :: Word64)
    , bench "xshowlp"  $ nf xshowlp  (0x1f :: Word64)
    , bench "xshowlu"  $ nf xshowlu  (0x1f :: Word64)
    , bench "xshowlpu" $ nf xshowlpu (0x1f :: Word64)
    , bench "xshow"    $ nf xshow    (0x1f :: Word64)
    , bench "xshowp"   $ nf xshowp   (0x1f :: Word64)
    , bench "xshowu"   $ nf xshowu   (0x1f :: Word64)
    , bench "xshowpu"  $ nf xshowpu  (0x1f :: Word64)
    ]
  ]
