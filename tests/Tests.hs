{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where
import Data.Index
import Data.Monoid
import Test.QuickCheck
import System.Exit

type Index = 10:.10:.10:.Z

index :: Proxy Index
index = Proxy

ix :: Proxy (4:.4:.Z)
ix = [dim|4 4|]

i10 :: Index -> Index
i10 = id

prop_maxBound = i10 maxBound == each 9
prop_minBound = i10 minBound == each 0

prop_plusZero = i10 zero == zero + 0

prop_fromIndexWrapY (mul :: Int) =
  fromIndex (10*y) == i10 (0:.y:.0:.Z)
 where
  y = mul `mod` 10

prop_fromIndexWrapZ (mul :: Int) =
  fromIndex (100*z) == i10 (0:.0:.z:.Z)
 where
  z = mul `mod` 10

prop_fromIndexWrapYZ (ym :: Int) (zm :: Int) =
  fromIndex (10*y + 100*z) == i10 (0:.y:.z:.Z)
 where
  y = ym `mod` 10
  z = zm `mod` 10

prop_fromIndexWrapXYZ (xm :: Int) (ym :: Int) (zm :: Int) =
  fromIndex (x + 10*y + 100*z) == i10 (x:.y:.z:.Z)
 where
  x = xm `mod` 10
  y = ym `mod` 10
  z = zm `mod` 10

prop_fromIndexM1 =
  fromIndex (-1) == i10 maxBound .&&.
  fromIndex 999  == i10 maxBound

prop_rangeUR = range [dimu|4 4|] == range [dimr|4 4|]

prop_rangeIndices =
  foldrRangeIndices (roll ix) (:) [] == foldrRangeIndices (unroll ix) (:) []

prop_rank = rank (maxBound `asProxyTypeOf` index) == 3

prop_rangeIndicesToIndexRolled =
  foldrRangeIndices (roll ix) (:) [] ==
  map toIndex (foldrRange (roll ix) (:) [])

prop_rangeIndicesToIndexUnrolled =
  foldrRangeIndices (unroll ix) (:) [] ==
  map toIndex (foldrRange (unroll ix) (:) [])

prop_fromIndexIndices =
  map fromIndex (foldrRangeIndices (roll ix) (:) []) ==
  foldrRange (roll ix) (:) []
       
return []

main :: IO ()
main = exitWith . boolExit =<< $quickCheckAll
 where
  boolExit True  = ExitSuccess
  boolExit False = ExitFailure 0
