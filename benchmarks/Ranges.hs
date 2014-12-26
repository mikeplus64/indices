{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
import Control.DeepSeq
import Criterion.Main
import Data.Index
import Data.Proxy

instance NFData Z
instance NFData (x:.xs)

{-# INLINE unrollRange #-}
unrollRange :: Ranged a => Proxy a -> [a]
unrollRange _ = range Unroll

{-# NOINLINE unrollRangeNoinline #-}
unrollRangeNoinline :: Ranged a => Proxy a -> [a]
unrollRangeNoinline _ = range Unroll

{-# INLINE rollRange #-}
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

{-# INLINE rollWithRange #-}
rollWithRange :: Dim n => Proxy n -> IO ()
rollWithRange r = withRange (roll r) test

{-# NOINLINE rollWithRangeNoinline #-}
rollWithRangeNoinline :: Dim n => Proxy n -> IO ()
rollWithRangeNoinline r = withRange (roll r) test

main :: IO ()
main = defaultMain
  [ benchWithRanges [dim|2 2|]
  , benchWithRanges [dim|4 4|]
  , benchRanges [dim|2 2|]
  , benchRanges [dim|4 4|]
  ]

{-# INLINE benchWithRanges #-}
benchWithRanges :: (Show r, NFData r, Ranged r) => Proxy r -> Benchmark
benchWithRanges r
  = bgroup (showBound r)
    [ bench "rollWithRange"           . whnfIO $ rollWithRange r
    , bench "rollWithRangeNoinline"   . whnfIO $ rollWithRangeNoinline r
    , bench "unrollWithRange"         . whnfIO $ unrollWithRange r
    , bench "unrollWithRangeNoinline" . whnfIO $ unrollWithRangeNoinline r
    ]

{-# INLINE benchRanges #-}
benchRanges :: (Show r, NFData r, Ranged r) => Proxy r -> Benchmark
benchRanges r
  = bgroup (showBound r)
    [ bench "rollRange"            $ nf rollRange r
    , bench "rollRangeNoinline"    $ nf rollRangeNoinline r
    , bench "unrollRange"          $ nf unrollRange r
    , bench "unrollRangeNoinline"  $ nf unrollRangeNoinline r
    ]
