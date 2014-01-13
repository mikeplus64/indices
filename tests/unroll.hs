{-# LANGUAGE QuasiQuotes, DataKinds, BangPatterns, TypeOperators, PolyKinds #-}
import Data.Index
import Criterion.Main
import Control.DeepSeq
import Data.Proxy

instance NFData Z
instance NFData b => NFData (a:.b) where
  rnf (a:.b) = a `seq` rnf b

staticRange :: Ranged a => Proxy a -> [a]
staticRange _ = srange

{-# NOINLINE staticRangeSlow #-}
staticRangeSlow :: Ranged a => Proxy a -> [a]
staticRangeSlow _ = srange

runtimeRange :: Dim a => Proxy a -> [a]
runtimeRange _ = range

{-# NOINLINE runtimeRangeSlow #-}
runtimeRangeSlow :: Dim a => Proxy a -> [a]
runtimeRangeSlow _ = range

{-# NOINLINE test #-}
test :: a -> IO ()
test = return (return ())

staticWithRange :: Ranged n => Proxy n -> IO ()
staticWithRange r = swithRange r test

{-# NOINLINE staticWithRangeSlow #-}
staticWithRangeSlow :: Ranged n => Proxy n -> IO ()
staticWithRangeSlow r = swithRange r test

runtimeWithRange :: Dim n => Proxy n -> IO ()
runtimeWithRange r = withRange r test

{-# NOINLINE runtimeWithRangeSlow #-}
runtimeWithRangeSlow :: Dim n => Proxy n -> IO ()
runtimeWithRangeSlow r = withRange r test

main :: IO ()
main = defaultMain 
  [ bgroup "withRanges"
    [ benchWithRanges [dim|2 2|]
    , benchWithRanges [dim|4 4|]
    , benchWithRanges [dim|8 8|]
    , benchWithRanges [dim|16 16|]
    ]
  , bgroup "ranges"
    [ benchRanges [dim|2 2|]
    , benchRanges [dim|4 4|]
    , benchRanges [dim|8 8|]
    , benchRanges [dim|16 16|]
    ]
  ]

{-# INLINE benchWithRanges #-} 
benchWithRanges :: (Show r, NFData r, Ranged r) => Proxy r -> Benchmark
benchWithRanges r
  = bgroup (show (reflect `asProxyTypeOf` r))
    [ bench "runtimeWithRange" (runtimeWithRange r)
    , bench "runtimeWithRangeSlow" (runtimeWithRangeSlow r)
    , bench "staticWithRange" (staticWithRange r)
    , bench "staticWithRangeSlow" (staticWithRangeSlow r)
    ]

{-# INLINE benchRanges #-}
benchRanges :: (Show r, NFData r, Ranged r) => Proxy r -> Benchmark
benchRanges r
  = bgroup (show (reflect `asProxyTypeOf` r))
    [ bench "runtimeRange" (runtimeRange `nf` r)
    , bench "runtimeRangeSlow" (runtimeRangeSlow `nf` r)
    , bench "staticRange" (staticRange `nf` r)
    , bench "staticRangeSlow" (staticRangeSlow `nf` r)
    ]
