{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternGuards #-}
-- | The indices supplied by this module are static type and value-level linked
-- lists. Since their type gives us information about them, recursion on indices
-- can be unrolled easily.
--
-- Indices look like @ x:.y:.Z :: i:.j:.Z @. The value is the actual index used,
-- and the type is the upper-bound on that index.
--
-- For instance, one index of a 4x4 matrix is @ 2:.2:.Z :: 3:.3:.Z @, and
-- another index for a 1024x1024x1024 cube is
-- @ 512:.512:.512:.Z :: 1024:.1024:.1024:.Z @
--
module Data.Index
  ( -- * Core
       ,prev')
  , Rank(..)
  , (:.)(..), Z(..)
    -- * Selecting whether to unroll loops
  , Mode(..), roll, unroll, modeDim
    -- * Using ranges
  , foldlRange
  , foldrRange
  , withRange 
    -- ** Over 'Int' indices
  , foldlRangeIndices
  , foldrRangeIndices
  , withRangeIndices
    -- * Range types
  , Ranged
  , InRange
  , Range()
  , Peano(..)
  , ToPeano
  , Size
    -- * Utility
  , bounds
  , range
  , dimHead, dimTail
  , pdimHead, pdimTail
  , cnat
  , And
    -- * Syntax
  , dim, dimu, dimr
  , module Data.Proxy
  , hello
  ) where

import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import Data.Tagged
import qualified Data.Ix as Ix
import Data.Monoid (Monoid(..))
import Data.Data (Typeable, Data)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Control.Applicative
import Language.Haskell.TH hiding (Range(..))
import Language.Haskell.TH.Quote

-- | Index constructor, analogous to ':'
--
-- The 'Applicative' and 'Monad' instances multiply in their bind operations,
-- and their 'return'/'pure' creates an index whose first dimension is 1.
data a :. b = {-# UNPACK #-} !Int :. !b
  deriving (Show,Read,Eq,Ord,Generic,Functor,Foldable,Traversable)

-- | The zero index, used to end indices, just as '[]' ends a list.
data Z = Z
  deriving (Show,Read,Eq,Ord,Typeable,Data,Generic)

infixr 9 :.

class Rank a b where
  -- | Retain the rank, but change the upper bound
  setBound :: a -> b

instance Rank Z Z where
  {-# INLINE setBound #-}
  setBound _ = Z

-- | Rank
instance Rank xs ys => Rank (x:.xs) (y:.ys) where
  {-# INLINE setBound #-}
  setBound (x:.xs) = x:.setBound xs

class Ord n => Dim n where
  -- | All zeros
  zero         :: n
  -- | All ones
  unit         :: n
  -- | The number of dimensions in an index
  rank         :: n -> Int
  -- | The size of the index
  size         :: proxy n -> Int
  -- | Reify a type-level index into a value-level one
  reflect'     :: proxy n -> n
  -- | Reify a type-level index into a value-level one
  {-# INLINE reflect #-}
  reflect      :: n
  reflect      = reflect' Proxy
  -- | Increment by one.
  next         :: n -> n
  -- | Decrement by one.
  prev         :: n -> n
  -- | Same as 'succ', but there are no boundary checks, so when 'maxBound' is
  -- hit, it will wrap around to 'minBound' / 'zero'.
  next'        :: n -> n
  -- | Same as 'pred', but there are no boundary checks, so when 'minBound'
  -- / 'zero' is hit, it will wrap around to 'maxBound'.
  prev'        :: n -> n
  -- | Create an 'Int' index.
  toIndex      :: n -> Int
  fromIndex'   :: proxy n -> Int -> n
  {-# INLINE fromIndex #-}
  -- | Create an index from its 'Int' representation.
  fromIndex    :: Int -> n
  fromIndex    = fromIndex' Proxy
  -- | Ensure an index is within its bounds.
  correct      :: n -> n
  -- | Ensure the "head" dimension is within its bounds.
  correctOnce  :: n -> n
  -- | Alter the "head" dimension.
  overHead     :: (Int -> Int) -> n -> n
  -- | See 'maxBound'
  lastDim      :: proxy n -> n
  -- | Get the minimum values of two indices at each dimension
  zipMin       :: n -> n -> n

instance Dim Z where
  {-# INLINE zero #-}
  {-# INLINE unit #-}
  {-# INLINE rank #-}
  {-# INLINE size #-}
  {-# INLINE reflect' #-}
  {-# INLINE next #-}
  {-# INLINE prev #-}
  {-# INLINE next' #-}
  {-# INLINE prev' #-}
  {-# INLINE toIndex #-}
  {-# INLINE fromIndex' #-}
  {-# INLINE correct #-}
  {-# INLINE correctOnce #-}
  {-# INLINE overHead #-}
  {-# INLINE lastDim #-}
  {-# INLINE zipMin #-}
  zero           = Z
  unit           = Z
  rank         _ = 0
  size         _ = 1
  reflect'     _ = Z
  next         _ = error "next: Z"
  prev         _ = error "prev: Z"
  next'        _ = Z
  prev'        _ = Z
  toIndex      _ = 0
  fromIndex' _ i | i > 0      = error "fromIndex: index too large" 
                 | otherwise  = Z
  correct      _ = Z
  correctOnce  _ = Z
  overHead   _ _ = Z
  lastDim      _ = Z
  zipMin     _ _ = Z
instance (KnownNat x, Dim xs) => Dim (x:.xs) where
  {-# INLINE zero #-}
  {-# INLINE unit #-}
  {-# INLINE rank #-}
  {-# INLINE size #-}
  {-# INLINE reflect' #-}
  {-# INLINE next #-}
  {-# INLINE prev #-}
  {-# INLINE toIndex #-}
  {-# INLINE fromIndex' #-}
  {-# INLINE correct #-}
  {-# INLINE correctOnce #-}
  {-# INLINE overHead #-}
  {-# INLINE lastDim #-}
  {-# INLINE zipMin #-}
  zero               = 0 :. zero
  unit               = 1 :. unit
  rank       (_:.xs) = 1 + rank xs
  size             d = pdimHead d * size (pdimTail d)
  reflect'         d = pdimHead d :. reflect' (pdimTail d)
  next d@(x:.xs)
    | x < dimHead d - 1         = (x+1) :. xs
    | xs' <- next xs, xs' > zero = 0 :. xs'
    | otherwise                 = error "next: index already at maxBound"
  prev (x:.xs)
    | x > 0     = (x-1) :. xs
    | xs > zero = 0 :. prev xs
    | otherwise = error "prev: index below zero"
  next' d@(x:.xs)
    | x < dimHead d - 1 = (x+1) :. xs
    | otherwise         = 0 :. next' xs
  prev' (x:.xs)
    | x > 0     = (x-1) :. xs
    | xs > zero = 0 :. prev xs
    | otherwise = lastDim Proxy
  toIndex   d@(x:.xs) = x + dimHead d * toIndex xs
  fromIndex' d        = \ix -> (ix `mod` h) :. fromIndex' (pdimTail d) (ix `div` h) 
    where h = pdimHead d
  correct d@(x:.xs) 
    | x < dimHead d = x:.correct xs
    | otherwise     = error "correct: index too large" 
  correctOnce d@(x:.xs) 
    | x < dimHead d = x:.xs
    | otherwise     = error "correctOnce: index too large" 
  overHead f (x:.xs)  = f x :. xs
  lastDim           d = (pdimHead d - 1) :. lastDim (pdimTail d)
  zipMin (x:.xs) (y:.ys)  = min x y :. zipMin xs ys

-- | Select whether to generate an unrolled loop or just the loop at
-- compile-time.
data Mode :: * -> * where
  Unroll :: Ranged i => Mode i
  Roll   :: Dim i => Mode i

unroll :: Ranged a => Proxy a -> Mode a
unroll _ = Unroll

roll :: Dim a => Proxy a -> Mode a
roll _ = Roll

modeDim :: Mode a -> Proxy a
modeDim _ = Proxy

{-# INLINE range #-}
-- | The range of an index
range :: Mode n -> [n]
range mode = foldrRange mode (:) []

{-# INLINE proxyOf #-}
proxyOf :: a -> Proxy a
proxyOf = const Proxy

{-# INLINE foldrRange #-}
-- | Lazy right fold over a range.
foldrRange :: Mode n -> (n -> b -> b) -> b -> b
foldrRange Roll cons nil = go zero
 where
  top = lastDim Proxy -- hopefully make sure this isn't recomputed
  go !i
    | i < top   = i `cons` go (next' i)
    | otherwise = cons top nil
foldrRange Unroll cons nil = sfoldrRange_ (tagPeano zero) cons nil

{-# INLINE foldlRange #-}
-- | Lazy right fold over a range.
foldlRange :: Mode n -> (b -> n -> b) -> b -> b
foldlRange Roll f = go zero
 where
  top = lastDim Proxy -- hopefully make sure this isn't recomputed
  go !i !acc
    | i < top   = go (next' i) (f acc i)
    | otherwise = f acc i
foldlRange Unroll f = sfoldlRange_ (tagPeano zero) f

{-# INLINE withRange #-}
-- | Compute something from a range
withRange :: Applicative m => Mode a -> (a -> m ()) -> m ()
withRange m f = foldrRange m (\d acc -> acc *> f d) (pure ())

{-# INLINE foldlRangeIndices #-}
-- | Strict left fold over the /raw/ 'Int' indices under a range
foldlRangeIndices :: Mode n -> (b -> Int -> b) -> b -> b
foldlRangeIndices m@Roll cons = go 0
  where
    s = size m
    go !i !acc
      | i < s     = go (i+1) (cons acc i)
      | otherwise = acc
foldlRangeIndices m@Unroll cons = sfoldlRangeIndices_ (tagPeanoI m) cons

{-# INLINE foldrRangeIndices #-}
-- | Lazy right fold over the /raw/ 'Int' indices under a range
foldrRangeIndices :: Mode n -> (Int -> b -> b) -> b -> b
foldrRangeIndices m@Roll cons nil = go 0
  where
    s = size m
    go !i
      | i < s     = i `cons` go (i+1)
      | otherwise = nil
foldrRangeIndices m@Unroll cons nil = sfoldrRangeIndices_ (tagPeanoI m) cons nil

{-# INLINE withRangeIndices #-}
-- | Compute something using the /raw/ indices under a range
withRangeIndices :: Applicative m => Mode n -> (Int -> m ()) -> m ()
withRangeIndices m f = foldrRangeIndices m (\d acc -> acc *> f d) (pure ())

instance Num Z where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE signum #-}
  {-# INLINE abs #-}
  {-# INLINE fromInteger #-}
  _ + _         = Z
  _ - _         = Z
  _ * _         = Z
  negate      _ = Z
  signum      _ = Z
  abs         _ = Z
  fromInteger _ = Z
instance (Num xs, Dim (x:.xs)) => Num (x:.xs) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE signum #-}
  {-# INLINE abs #-}
  {-# INLINE fromInteger #-}
  x:.xs + y:.ys  = correctOnce ((x+y) :. (xs+ys))
  -- amusingly stylish-haskell thinks this is N+K patterns
  x:.xs - y:.ys  = correctOnce ((x-y) :. (xs-ys))
  x:.xs * y:.ys  = correctOnce ((x*y) :. (xs*ys))
  negate (x:.xs) = negate x :. negate xs
  signum (x:.xs) = signum x :. signum xs
  abs (x:.xs)    = abs x    :. abs xs
  fromInteger    = fromIndex . fromIntegral
instance Real Z where
  {-# INLINE toRational #-}
  toRational _ = 0
instance (Num (x:.xs), Dim (x:.xs)) => Real (x:.xs) where
  {-# INLINE toRational #-}
  toRational = toRational . toIndex
instance Enum Z where
  {-# INLINE toEnum #-}
  {-# INLINE fromEnum #-}
  {-# INLINE succ #-}
  {-# INLINE pred #-}
  {-# INLINE enumFromThen #-}
  {-# INLINE enumFromTo #-}
  {-# INLINE enumFromThenTo #-}
  toEnum             0 = Z
  toEnum             _ = error "toEnum: must be 0"
  fromEnum           _ = 0
  succ               _ = error "succ: Z"
  pred               _ = error "pred: Z"
  enumFrom           _ = [Z]
  enumFromThen     _ _ = [Z]
  enumFromTo       _ _ = [Z]
  enumFromThenTo _ _ _ = [Z]
instance (Dim (x:.xs), Num xs) => Enum (x:.xs) where
  {-# INLINE toEnum #-}
  {-# INLINE fromEnum #-}
  {-# INLINE succ #-}
  {-# INLINE pred #-}
  {-# INLINE enumFromThen #-}
  {-# INLINE enumFromTo #-}
  {-# INLINE enumFromThenTo #-}
  toEnum               = fromIndex
  fromEnum             = toIndex
  succ                 = next
  pred                 = prev
  enumFrom !l
    | l < maxBound = l : enumFrom (next' l)
    | otherwise    = [l]
  enumFromThen !l !j
    | l < maxBound = l : enumFromThen (l+j) j
    | otherwise    = [l]
  enumFromTo !l !h
    | l < h     = l : enumFromTo (next' l) h
    | otherwise = [h]
  -- todo: working enumFromThenTo
instance Integral Z where
  {-# INLINE div #-}
  {-# INLINE mod #-}
  {-# INLINE quot #-}
  {-# INLINE rem #-}
  {-# INLINE quotRem #-}
  {-# INLINE divMod #-}
  {-# INLINE toInteger #-}
  div     _ _ = Z
  mod     _ _ = Z
  quot    _ _ = Z
  rem     _ _ = Z
  quotRem _ _ = (Z, Z)
  divMod  _ _ = (Z, Z)
  toInteger _ = 0
instance (Integral xs, Dim (x:.xs)) => Integral (x:.xs) where
  {-# INLINE div #-}
  {-# INLINE mod #-}
  {-# INLINE quot #-}
  {-# INLINE rem #-}
  {-# INLINE quotRem #-}
  {-# INLINE divMod #-}
  {-# INLINE toInteger #-}
  div  (x:.xs) (y:.ys) = div x y :. div xs ys
  mod  (x:.xs) (y:.ys) = mod x y :. mod xs ys
  quot (x:.xs) (y:.ys) = quot x y :. quot xs ys
  rem  (x:.xs) (y:.ys) = rem x y :. rem xs ys
  quotRem xs ys        = (quot xs ys, rem xs ys)
  divMod xs ys         = (div xs ys, mod xs ys)
  toInteger            = toInteger . toIndex
instance Bounded Z where
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
  minBound = Z
  maxBound = Z
instance Dim (x:.xs) => Bounded (x:.xs) where
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
  minBound = zero
  maxBound = lastDim Proxy

instance Monoid Z where
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  mempty           = Z
  mappend _ _ = Z
instance (Dim (x:.xs), Monoid xs) => Monoid (x:.xs) where
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  mempty                  = zero
  mappend (x:.xs) (y:.ys) = correctOnce ((x+y):.mappend xs ys)
instance KnownNat s => Applicative ((:.) s) where
  pure x                    = 1:.x
  d@(ix0 :. f) <*> ix1 :. x = ((ix0*ix1) `rem` dimHead d) :. f x
  (*>)                      = (>>)
  (<*)                      = flip (>>)

instance KnownNat s => Monad ((:.) s) where
  return x              = 1:.x
  d@(ix0:.a) >>= f      = case f a of
    ix1 :. b -> ((ix0*ix1) `mod` dimHead d) :. b

instance Ix.Ix Z where
  {-# INLINE range #-}
  {-# INLINE index #-}
  {-# INLINE inRange #-}
  {-# INLINE rangeSize #-}
  range _ = [Z]
  index   _ _ = 0
  inRange _ _ = True
  rangeSize _ = 0

-- | The indices in an Ix instance are always bound by (0, t), where t is the
-- type of the index.
instance (Ranged (x:.xs), Num xs) => Ix.Ix (x:.xs) where
  {-# INLINE range #-}
  {-# INLINE index #-}
  {-# INLINE inRange #-}
  {-# INLINE rangeSize #-}
  range _         = range Unroll
  index _       c = toIndex c
  inRange _     c = minBound <= c && c <= maxBound
  rangeSize (_,b) = size (proxyOf b)

dimQQ val ty =  QuasiQuoter
  { quoteExp  = \s -> [| $val :: $(quoteType dim s) |]
  , quoteType = \s ->
      let cons a b = [t| $(litT $ numTyLit a) :. $b |]
          nil      = [t| Z |]
      in [t| $ty $(foldr cons nil . map read . words $ s) |]
  , quotePat  = error "dim in pattern context"
  , quoteDec  = error "dim in declaration context"
  }
                            
-- | Expands to a 'Proxy' with the phantom type being the dimension specified.
-- Works in types and expressions.
--
-- Examples:
--
-- @ id [dim|3 4 5|] ==> id (Proxy :: Proxy (3:.4:.5:.Z)) @
--
-- @ Proxy :: [dim|3 4 5|] ==> Proxy :: Proxy (3:.4:.5:.Z) @
--
dim :: QuasiQuoter
dim = dimQQ [| Proxy |] [t| Proxy |]

-- | Same as 'dim', but create an 'Unroll' instead of a 'Proxy'.
dimu :: QuasiQuoter
dimu = dimQQ [| Unroll |] [t| Mode |]

-- | Same as 'dim', but create a 'Roll' instead of a 'Proxy'.
dimr :: QuasiQuoter
dimr = dimQQ [| Roll |] [t| Mode |]

{-# INLINE dimHead #-}
dimHead :: KnownNat x => x:.xs -> Int
dimHead = cnat . proxyHead
  where proxyHead :: x:.xs -> Proxy x
        proxyHead _ = Proxy

{-# INLINE dimTail #-}
dimTail :: x:.xs -> xs
dimTail (_:.xs) = xs

{-# INLINE pdimHead #-}
pdimHead :: KnownNat x => proxy (x:.xs) -> Int
pdimHead = cnat . proxyHead
  where proxyHead :: proxy (x:.xs) -> Proxy x
        proxyHead _ = Proxy

{-# INLINE pdimTail #-}
pdimTail :: proxy (x:.xs) -> Proxy xs
pdimTail _ = Proxy

-- | Types that support static range operations
class (Dim n, Range (ToPeano (Size n))) => Ranged n
instance (Dim n, Range (ToPeano (Size n))) => Ranged n

-- | Peano numbers
data Peano = Zero | Succ Peano

-- | Convert a 'Nat' to a type-level 'Peano'
type family ToPeano (n :: Nat) :: Peano where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n-1))

-- | Compute the size of an index
type family Size (dim :: *) :: Nat where
  Size (x:.Z)  = x
  Size (x:.xs) = x * Size xs

class Range (n :: Peano) where
  swithRange_  :: (Dim o, Applicative m) => Tagged n o -> (o -> m ()) -> m ()
  sfoldrRange_ :: Dim o => Tagged n o -> (o -> b -> b) -> b -> b
  sfoldlRange_ :: Dim o => Tagged n o -> (b -> o -> b) -> b -> b

  swithRangeIndices_  :: Applicative m => Tagged n Int -> (Int -> m ()) -> m ()
  sfoldrRangeIndices_ :: Tagged n Int -> (Int -> b -> b) -> b -> b
  sfoldlRangeIndices_ :: Tagged n Int -> (b -> Int -> b) -> b -> b

instance Range Zero where
  {-# INLINE swithRange_ #-}
  {-# INLINE sfoldrRange_ #-}
  {-# INLINE sfoldlRange_ #-}
  swithRange_  _ _   = pure ()
  sfoldrRange_ _ _ z = z
  sfoldlRange_ _ _ z = z
  {-# INLINE swithRangeIndices_ #-}
  {-# INLINE sfoldrRangeIndices_ #-}
  {-# INLINE sfoldlRangeIndices_ #-}
  swithRangeIndices_  _ _    = pure ()
  sfoldrRangeIndices_ _ _  z = z
  sfoldlRangeIndices_ _ _ !z = z
  
instance Range n => Range (Succ n) where
  {-# INLINE swithRange_ #-}
  {-# INLINE sfoldrRange_ #-}
  {-# INLINE sfoldlRange_ #-}
  swithRange_  !i f    = f (unTagged i) *> swithRange_ (nextTagged i) f
  sfoldrRange_  i f  z = f (unTagged i) (sfoldrRange_ (nextTagged i) f z)
  sfoldlRange_ !i f !z = sfoldlRange_ (nextTagged i) f (f z (unTagged i))

  {-# INLINE swithRangeIndices_ #-}
  {-# INLINE sfoldrRangeIndices_ #-}
  {-# INLINE sfoldlRangeIndices_ #-}
  swithRangeIndices_  !i f    = f (unTagged i) *> swithRangeIndices_ (nextTaggedI i) f
  sfoldrRangeIndices_  i f  z = f (unTagged i) (sfoldrRangeIndices_ (nextTaggedI i) f z)
  sfoldlRangeIndices_ !i f !z = sfoldlRangeIndices_ (nextTaggedI i) f (f z (unTagged i))

{-# INLINE nextTagged #-}
nextTagged :: Dim a => Tagged (Succ n) a -> Tagged n a
nextTagged (Tagged a) = Tagged (next' a)

{-# INLINE nextTaggedI #-}
nextTaggedI :: Tagged (Succ n) Int -> Tagged n Int
nextTaggedI (Tagged a) = Tagged (a+1)

{-# INLINE tagPeano #-}
tagPeano :: Dim n => n -> Tagged (ToPeano (Size n)) n
tagPeano = Tagged

{-# INLINE tagPeanoI #-}
tagPeanoI :: proxy n -> Tagged (ToPeano (Size n)) Int
tagPeanoI _ = Tagged 0

-- | Create a bound for use with e.g. "Data.Array.array"
bounds :: (Dim a, Bounded a) => Proxy a -> (a, a)
bounds _ = (zero, maxBound)

-- | @ fromInteger . natVal @
cnat :: KnownNat n => proxy (n :: Nat) -> Int
cnat = fromInteger . natVal

