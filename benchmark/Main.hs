import Criterion.Main

import Hexy

import Data.Word (Word32)
import Numeric (showHex)
import Text.Printf (printf)

main :: IO ()
main = defaultMain
  [ bgroup "Word32"
    [ bench "printf"  $ nf (printf "%08x" :: Word32 -> String) 0x1f
    , bench "showHex" $ nf (showHex (0x1f :: Word32)) ""
    , bench "xshow"   $ nf xshow    (0x1f :: Word32)
    , bench "xshowp"  $ nf xshowp   (0x1f :: Word32)
    , bench "xshowu"  $ nf xshowu   (0x1f :: Word32)
    , bench "xshowpu" $ nf xshowpu  (0x1f :: Word32)
    ]
  ]
