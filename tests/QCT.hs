{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}
module QCT where
import Data.Index
import Test.QuickCheck
import Test.QuickCheck.All
import QCUtil

proxify :: n -> Proxy n
proxify _ = Proxy

prop_maxBound :: 10:.10:.10:.Z -> Bool
prop_maxBound n = maxBound == reflect `asTypeOf` n - unit

prop_minBound :: 10:.10:.10:.Z -> Bool
prop_minBound n = zero == minBound `asTypeOf` n

prop_zeroPlusN :: 10:.10:.10:.Z -> Bool
prop_zeroPlusN n = zero + n - n == zero

prop_index :: 10:.10:.10:.Z -> Bool
prop_index n = toIndex n < size (proxify n)

p4 :: 10:.10:.10:.Z -> Bool
p4 n = n - n == zero

p5 :: 10:.10:.10:.Z -> Bool
p5 n = n + n - n == n

main :: IO ()
main = $quickCheckAll >>= print
