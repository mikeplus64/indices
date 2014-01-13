module Data.Index.Tests where
import Test.QuickCheck (Arbitrary(..), Gen)

instance Arbitrary Z where
  arbitrary = pure Z
instance (Dim (x:.xs), CNat x, Num xs, Arbitrary xs) => Arbitrary ((x::Nat):.xs) where
  arbitrary = f (Proxy :: Proxy (x:.xs))
    where
      f :: (CNat x, Arbitrary xs) => Proxy (x:.xs) -> Gen (x:.xs)
      f p = do
        x  <- fmap (`mod` pdimHead p) arbitrary
        xs <- arbitrary
        pure (x:.xs)
