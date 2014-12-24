{-# LANGUAGE QuasiQuotes, DataKinds, BangPatterns, TypeOperators, PolyKinds #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
import Data.Index
import Criterion.Main
import Control.DeepSeq
import Data.Proxy

instance NFData Z
instance NFData b => NFData (a:.b) where
  rnf (a:.b) = a `seq` rnf b

{-# INLINE unrollRange #-}
unrollRange :: Ranged a => Proxy a -> [a]
unrollRange _ = range Unroll

{-# NOINLINE unrollRangeNoinline #-}
unrollRangeNoinline :: Ranged a => Proxy a -> [a]
unrollRangeNoinline _ = range Unroll

rollRange :: Dim a => Proxy a -> [a]
rollRange _ = range Roll

{-# NOINLINE rollRangeNoinline #-}
rollRangeNoinline :: Dim a => Proxy a -> [a]
rollRangeNoinline _ = range Roll

{-# NOINLINE test #-}
test :: a -> IO ()
test = return (return ())

{-# INLINE unrollWithRange #-}
unrollWithRange :: Ranged n => Proxy n -> IO ()
unrollWithRange r = withRange (unroll r) test

{-# NOINLINE unrollWithRangeNoinline #-}
unrollWithRangeNoinline :: Ranged n => Proxy n -> IO ()
unrollWithRangeNoinline r = withRange (unroll r) test

rollWithRange :: Dim n => Proxy n -> IO ()
rollWithRange r = withRange (roll r) test

{-# NOINLINE rollWithRangeNoinline #-}
rollWithRangeNoinline :: Dim n => Proxy n -> IO ()
rollWithRangeNoinline r = withRange (roll r) test

main :: IO ()
main = defaultMain 
  [ bgroup "withRanges"
    [ benchWithRanges [dim|2 2|]
    , benchWithRanges [dim|4 4|]
    ]
  , bgroup "ranges"
    [ benchRanges [dim|2 2|]
    , benchRanges [dim|4 4|]
    ]
  ]

{-# INLINE benchWithRanges #-} 
benchWithRanges :: (Show r, NFData r, Ranged r) => Proxy r -> Benchmark
benchWithRanges r
  = bgroup (show (reflect `asProxyTypeOf` r))
    [ bench "rollWithRange"           . whnfIO $ rollWithRange r
    , bench "rollWithRangeNoinline"   . whnfIO $ rollWithRangeNoinline r
    , bench "unrollWithRange"         . whnfIO $ unrollWithRange r
    , bench "unrollWithRangeNoinline" . whnfIO $ unrollWithRangeNoinline r
    ]

{-# INLINE benchRanges #-}
benchRanges :: (Show r, NFData r, Ranged r) => Proxy r -> Benchmark
benchRanges r
  = bgroup (show (reflect `asProxyTypeOf` r))
    [ bench "rollRange"            $ whnf rollRange r
    , bench "rollRangeNoinline"    $ whnf rollRangeNoinline r
    , bench "unrollRange"          $ whnf unrollRange r
    , bench "unrollRangeNoinline"  $ whnf unrollRangeNoinline r
    ]
