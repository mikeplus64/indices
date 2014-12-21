{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Index.Nat (cnat, module TypeLits) where
import GHC.TypeLits as TypeLits

cnat :: KnownNat n => proxy (n :: Nat) -> Int
cnat = fromInteger . natVal
