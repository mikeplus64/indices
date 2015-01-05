{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Type.Nat where
import GHC.TypeLits

-- | Peano numbers
-- These are needed for some operations that I can't figure out how to express
-- with "GHC.TypeLits.Nat" due to 'Nat'\'s very small number of operations.
data Peano = Zero | Succ Peano

type family ToPeano (n :: Nat) :: Peano where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n - 1))

type family FromPeano (n :: Peano) :: Nat where
  FromPeano (Succ n) = 1 + FromPeano n
  FromPeano Zero     = 0

-- | Division by repeated subtraction
type family QuotRem (n :: Nat) (d :: Nat) where
  QuotRem n d = QuotRem' n d 0 n n (d <=? n)

type family QuotRem' n d q r' r rGtD where
  QuotRem' n d q r' r True  = QuotRem' n d (q+1) r (r-d) (d <=? r)
  QuotRem' n d q r' r False = '(q-1, r')

type family Fst (pair :: (a, b)) :: a where Fst '(a,b) = a
type family Snd (pair :: (a, b)) :: b where Snd '(a,b) = b

type family Quot a b where Quot a b = Fst (QuotRem a b)
type family Rem  a b where Rem  a b = Snd (QuotRem a b)


