{-# LANGUAGE DataKinds #-}
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
-- Parts of this module are alternatively under "Static" and "Runtime" headings.
-- Functions under "Static" headings should be inlined completely, and functions
-- under "Runtime" will not. If you do not compile with optimisations on, ghc
-- will not inline the "static" functions, which will then perform very poorly
-- compared to their "runtime" counterparts.

module Data.Index
  ( -- * Core
    Index
  , Dim(zero,unit,size,rank,correct,zipMin,toIndex,fromIndex,next'
       ,prev')
  , Rank(..)
  , (:.)(..), Z(..)
    -- * Ranges
  , Ranged
  , Range()
  , Peano(..)
  , ToPeano
  , Size
    -- ** Static
  , sfoldlRange
  , sfoldrRange
  , swithRange
    -- ** Runtime
  , foldlRange
  , foldrRange
  , withRange 
    -- ** Over 'Int' indices
    -- *** Static
  , sfoldlRangeIndices
  , sfoldrRangeIndices
  , swithRangeIndices
    -- *** Runtime
  , foldlRangeIndices
  , foldrRangeIndices
  , withRangeIndices
    -- * Utility
  , range
  , srange
  , dimHead, dimTail
  , pdimHead, pdimTail
    -- * Syntax
  , index
  , module Data.Proxy
  ) where

import Data.Index.Nat
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

-- | A single class encompassing all the instances an index should have.
class (Bounded n, Integral n, Show n, Read n, Ix.Ix n, Dim n) => Index n
instance (Bounded n, Integral n, Show n, Read n, Ix.Ix n, Dim n) => Index n

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
  size         :: Proxy n -> Int
  -- | Reify a type-level index into a value-level one
  reflect'     :: Proxy n -> n
  -- | Reify a type-level index into a value-level one
  {-# INLINE reflect #-}
  reflect      :: n
  reflect      = reflect' Proxy
  next         :: n -> n
  prev         :: n -> n
  -- | Same as 'succ', but there are no boundary checks, so when 'maxBound' is
  -- hit, it will wrap around to 'minBound' / 'zero'.
  next'        :: n -> n
  -- | Same as 'pred', but there are no boundary checks, so when 'minBound'
  -- / 'zero' is hit, it will wrap around to 'maxBound'.
  prev'        :: n -> n
  toIndex      :: n -> Int
  fromIndex'   :: Proxy n -> Int -> n
  {-# INLINE fromIndex #-}
  fromIndex    :: Int -> n
  fromIndex    = fromIndex' Proxy
  correct      :: n -> n
  correctOnce  :: n -> n
  overHead     :: (Int -> Int) -> n -> n
  lastDim      :: Proxy n -> n
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
instance (CNat x, Dim xs) => Dim (x:.xs) where
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

{-# INLINE range #-}
-- | The range of an index
--
-- @ range = foldrRange Proxy (:) [] @
range :: Dim n => [n]
range = foldrRange Proxy (:) []

{-# INLINE srange #-}
-- | Statically generated range of an index
--
-- @ srange = sfoldrRange Proxy (:) [] @
srange :: Ranged n => [n]
srange = sfoldrRange Proxy (:) []

{-# INLINE proxyOf #-}
proxyOf :: a -> Proxy a
proxyOf = const Proxy

{-# INLINE foldlRange #-}
-- | Eager left fold over a range
--
-- @ 'foldlRange' r f z == 'Data.List.foldl'' f z ('asProxyTypeOf' 'range' r) @
foldlRange :: Dim a => Proxy a -> (b -> a -> b) -> b -> b
foldlRange _ cons = go zero
  where
    top = lastDim Proxy
    go !i !acc
      | i < top   = go (next' i) (cons acc i)
      | otherwise = cons acc top

{-# INLINE foldrRange #-}
-- | Lazy right fold over a range
--
-- @ 'foldrRange' r f z == 'Data.List.foldr' f z ('asProxyTypeOf' 'range' r) @
foldrRange :: Dim a => Proxy a -> (a -> b -> b) -> b -> b
foldrRange _ cons nil = go zero
  where
    top = lastDim Proxy
    go !i
      | i < top   = i `cons` go (next' i)
      | otherwise = cons top nil

{-# INLINE withRange #-}
-- | Compute something from a range
withRange :: (Applicative m, Dim a) => Proxy a -> (a -> m ()) -> m ()
withRange m f = foldrRange m (\d acc -> acc *> f d) (pure ())

{-# INLINE foldlRangeIndices #-}
-- | Strict left fold over the /raw/ indices under a range
foldlRangeIndices :: Dim a => Proxy a -> (b -> Int -> b) -> b -> b
foldlRangeIndices m cons = go 0
  where
    go !i !acc
      | i < size m = go (i+1) (cons acc i)
      | otherwise  = acc

{-# INLINE foldrRangeIndices #-}
-- | Lazy right fold over the /raw/ indices under a range
foldrRangeIndices :: Dim a => Proxy a -> (Int -> b -> b) -> b -> b
foldrRangeIndices m cons nil = go 0
  where
    go !i
      | i < size m = i `cons` go (i+1)
      | otherwise  = nil

{-# INLINE withRangeIndices #-}
-- | Compute something using the /raw/ indices under a range
withRangeIndices :: (Applicative m, Dim a) => Proxy a -> (Int -> m ()) -> m ()
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
instance CNat s => Applicative ((:.) s) where
  pure x                    = 1:.x
  d@(ix₁ :. f) <*> ix₂ :. x = ((ix₁*ix₂) `rem` dimHead d) :. f x
  (*>)                      = (>>)
  (<*)                      = flip (>>)
instance CNat s => Monad ((:.) s) where
  return x              = 1:.x
  d@(ix₁:.a) >>= f      = case f a of
    ix₂ :. b -> ((ix₁*ix₂) `mod` dimHead d) :. b

instance Ix.Ix Z where
  {-# INLINE range #-}
  {-# INLINE index #-}
  {-# INLINE inRange #-}
  {-# INLINE rangeSize #-}
  range   _   = [Z]
  index   _ _ = 0
  inRange _ _ = True
  rangeSize _ = 0
instance (Dim (x:.xs), Num xs) => Ix.Ix (x:.xs) where
  {-# INLINE range #-}
  {-# INLINE index #-}
  {-# INLINE inRange #-}
  {-# INLINE rangeSize #-}
  range           = uncurry enumFromTo
  index _       c = toIndex c
  inRange _     c = minBound < c && c < maxBound
  rangeSize (l,_) = size (Proxy `proxify` l)
    where
      {-# INLINE proxify #-}
      proxify :: Proxy a -> a -> Proxy a
      proxify p _ = p

-- | Expands to a 'Proxy' with the phantom type being the dimension specified
-- Works in types and expressions.
--
-- Examples:
--
-- @ id [index|3 4 5|] ==> id (Proxy :: Proxy (3:.4:.5:.Z)) @
--
-- @ Proxy :: [index|3 4 5|] ==> Proxy :: Proxy (3:.4:.5:.Z) @
--
index :: QuasiQuoter
index = QuasiQuoter
  { quoteExp  = \s -> [| Proxy :: $(quoteType index s) |]
  , quoteType = \s ->
      let cons a b = [t| $(litT $ numTyLit a) :. $b |]
          nil      = [t| Z |]
      in [t| Proxy $(foldr cons nil . map read . words $ s) |]
  , quotePat  = \_ -> [p| Proxy |] -- probably not what would be wanted
  , quoteDec  = error "dim"
  }

{-# INLINE dimHead #-}
dimHead :: CNat x => x:.xs -> Int
dimHead = cnat . proxyHead
  where proxyHead :: x:.xs -> Proxy x
        proxyHead _ = Proxy

{-# INLINE dimTail #-}
dimTail :: x:.xs -> xs
dimTail (_:.xs) = xs

{-# INLINE pdimHead #-}
pdimHead :: CNat x => Proxy (x:.xs) -> Int
pdimHead = cnat . proxyHead
  where proxyHead :: Proxy (x:.xs) -> Proxy x
        proxyHead _ = Proxy

{-# INLINE pdimTail #-}
pdimTail :: Proxy (x:.xs) -> Proxy xs
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
tagPeanoI :: Proxy n -> Tagged (ToPeano (Size n)) Int
tagPeanoI _ = Tagged 0

{-# INLINE swithRange #-}
-- | See 'withRange'
--
-- With optimisations, this compiles to an unrolled loop
swithRange :: (Applicative m, Ranged o) => Proxy o -> (o -> m ()) -> m ()
swithRange _ (f :: o -> m ()) = swithRange_ (Tagged zero :: Tagged (ToPeano (Size o)) o) f

{-# INLINE sfoldrRange #-}
-- | See 'foldrRange'
--
-- With optimisations, this compiles to an unrolled loop
sfoldrRange :: Ranged o => Proxy o -> (o -> b -> b) -> b -> b 
sfoldrRange _ (f :: o -> b -> b) z = sfoldrRange_ (tagPeano zero) f z

{-# INLINE sfoldlRange #-}
-- | See 'foldlRange'
--
-- With optimisations, this compiles to an unrolled loop
sfoldlRange :: Ranged o => Proxy o -> (b -> o -> b) -> b -> b 
sfoldlRange _ (f :: b -> o -> b) !z = sfoldlRange_ (tagPeano zero) f z

{-# INLINE swithRangeIndices #-}
-- | See 'withRangeIndices'
--
-- With optimisations, this compiles to an unrolled loop
swithRangeIndices :: (Applicative m, Ranged o) => Proxy o -> (Int -> m ()) -> m ()
swithRangeIndices dim f = swithRangeIndices_ (tagPeanoI dim) f

{-# INLINE sfoldrRangeIndices #-}
-- | See 'foldrRangeIndices'
--
-- With optimisations, this compiles to an unrolled loop
sfoldrRangeIndices :: Ranged o => Proxy o -> (Int -> b -> b) -> b -> b
sfoldrRangeIndices dim f z = sfoldrRangeIndices_ (tagPeanoI dim) f z

{-# INLINE sfoldlRangeIndices #-}
-- | See 'foldlRangeIndices'
--
-- With optimisations, this compiles to an unrolled loop
sfoldlRangeIndices :: Ranged o => Proxy o -> (b -> Int -> b) -> b -> b
sfoldlRangeIndices dim f z = sfoldlRangeIndices_ (tagPeanoI dim) f z
