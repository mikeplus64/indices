{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Index.Tests where
import           Control.Applicative
import           Data.Index
import           Data.Index.Nat
import qualified Data.Vector         as V
import           GHC.TypeLits
import           Test.QuickCheck


instance Arbitrary Z where
  arbitrary = pure Z

instance (Dim (x:.xs), CNat x, Num xs, Arbitrary xs) => Arbitrary ((x::Nat):.xs) where
  arbitrary = f (Proxy :: Proxy (x:.xs))
    where
      f :: (CNat x, Arbitrary xs) => Proxy (x:.xs) -> Gen (x:.xs)
      f p = do
        x  <- (`mod` pdimHead p) <$> arbitrary
        xs <- arbitrary
        pure (x:.xs)

assert :: String -> Bool -> IO ()
assert err False = error err
assert _   True  = return ()

v :: V.Vector Int
v = V.fromList [0..15]

{-# INLINE printIx #-}
printIx :: Int -> IO ()
printIx i = print (v V.! i)

main :: IO ()
main = do
  assert "range 2:.2:.Z"
    ((range :: [2:.2:.Z]) == [0:.0:.Z, 1:.0:.Z, 0:.1:.Z, 1:.1:.Z])

  assert "s range 2:.2:.Z"
    ((srange :: [2:.2:.Z]) == [0:.0:.Z, 1:.0:.Z, 0:.1:.Z, 1:.1:.Z])

  assert "indices 2:.2:.Z"
    (foldrRangeIndices [index|2 2|] (:) [] == [0,1,2,3])

  assert "s indices 2:.2:.Z"
    (sfoldrRangeIndices [index|2 2|] (:) [] == [0,1,2,3])

  swithRangeIndices [index|4 4|] printIx
