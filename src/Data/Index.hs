{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    Dim( each
       , size
       , rank
       , correct
       , toIndex
       , fromIndex
       , next
       , prev
       , lastDim)
  , zero
  , Rank(..)
  , (:.)(..), (.:), dimHead, dimTail, Z(..)
    -- * Selecting whether to unroll loops
  , Mode(..), unroll, unrollBy, roll, rollBy, modeProxy
    -- * Using ranges
  , foldlRange
  , foldrRange
  , withRange
    -- ** Over 'Int' indices
  , foldlRangeIndices
  , foldrRangeIndices
  , withRangeIndices
    -- * Algorithms
  , divideAndConquer
  , Slice(..)
  , DivideAndConquer
  , DMode(..), dunroll, droll
    -- * Range types
  , Ranged
  , InRange
  , Range()
  , Size
    -- * Peano numbers
  , Peano(..)
  , ToPeano
  , FromPeano
    -- * Utility
  , showBound
  , bounds
  , range
  , pdimHead, pdimTail
  , cnat
    -- ** Type families
  , DivideAll
  , Halves(..)
    -- * Syntax
  , dim, dimu, dimr
  , module Data.Proxy
  ) where

import           Control.Applicative
import           Data.Data                 (Data, Typeable)
import           Data.Foldable             (Foldable)
import qualified Data.Ix                   as Ix
import           Data.Monoid               (Monoid (..))
import           Data.Proxy
import           Data.Type.Bool
import           Data.Type.Nat
import           Data.Tagged
import           Data.Traversable          (Traversable)
import           GHC.Generics
import           GHC.TypeLits
import           Language.Haskell.TH       hiding (Range (..))
import           Language.Haskell.TH.Quote

-- | Index constructor, analogous to ':'
--
-- The 'Applicative' and 'Monad' instances multiply in their bind operations,
-- and their 'return'/'pure' creates an index whose first dimension is 1.
data (a :: Nat) :. b = {-# UNPACK #-} !Int :. !b
  deriving (Show,Read,Eq,Ord,Generic,Functor,Foldable,Traversable)

infixr 9 :.

{-# INLINE (.:) #-}
-- | Construct an index. This ensures that all constructed indices are valid by
-- taking the remainder. Negative indices go to 0.
(.:) :: forall (x :: Nat) xs. KnownNat x => Int -> xs -> x :. xs
(.:) x xs = (x `rem` cnat (Proxy :: Proxy x)) :. xs

infixr 9 .:

-- | The zero index, used to end indices, just as '[]' ends a list.
data Z = Z
  deriving (Show,Read,Eq,Ord,Typeable,Data,Generic)

-- | Show the upper bound, which indices cannot "reach".
showBound :: (Show a, Dim a) => Proxy a -> String
showBound p = show (unsafeMapDim (\_ a -> a+1) (lastDim p))

class Rank a b where
  -- | Retain the rank, but change the upper bound
  setBound :: a -> b

instance Rank Z Z where
  {-# INLINE setBound #-}
  setBound _ = Z

-- | Rank
instance (x <= y, Rank xs ys) => Rank (x:.xs) (y:.ys) where
  {-# INLINE setBound #-}
  setBound (x:.xs) = x:.setBound xs

{-# INLINE zero #-}
zero :: Dim a => a
zero = each 0

class (Bounded n, Ord n) => Dim n where
  -- | @each n@ = an index of all 'n'
  each         :: Int -> n

  -- | The number of dimensions in an index
  rank         :: n -> Int

  -- | The size of the index
  size         :: proxy n -> Int

  -- | Increment by one. Wraps around to 'minBound' when 'maxBound' is given.
  next         :: n -> n

  -- | Decrement by one. Wraps around to 'maxBound' when 'minBound' is given.
  prev         :: n -> n

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

  -- | See 'maxBound'.
  lastDim      :: proxy n -> n

  -- | Similar to 'zipWith'. This will take the modulus of your input.
  -- The first argument to the inputted function is the max bound at that index.
  zipDims :: (Int -> Int -> Int -> Int) -> n -> n -> n

  -- | Same as 'zipDims', but it doesn't check whether the 'Int's you generate
  -- are valid.
  -- The first argument to the inputted function is the max bound at that index.
  unsafeZipDims :: (Int -> Int -> Int -> Int) -> n -> n -> n

  -- | Map over indices. The first argument to the inputted functions is the
  -- bound at that index.
  mapDim :: (Int -> Int -> Int) -> n -> n

  -- | Map over indices. The first argument to the inputted functions is the
  -- bound at that index.
  -- The output is not checked to be valid.
  unsafeMapDim :: (Int -> Int -> Int) -> n -> n

instance Dim Z where
  {-# INLINE each #-}
  each _ = Z

  {-# INLINE rank #-}
  rank _ = 0

  {-# INLINE size #-}
  size _ = 1

  {-# INLINE next #-}
  next _ = Z

  {-# INLINE prev #-}
  prev _ = Z

  {-# INLINE toIndex #-}
  toIndex _ = 0

  {-# INLINE fromIndex' #-}
  fromIndex' _ _ = Z

  {-# INLINE correct #-}
  correct _ = Z

  {-# INLINE correctOnce #-}
  correctOnce _ = Z

  {-# INLINE lastDim #-}
  lastDim _ = Z

  {-# INLINE zipDims #-}
  zipDims _ _ _ = Z

  {-# INLINE unsafeZipDims #-}
  unsafeZipDims _ _ _ = Z

  {-# INLINE mapDim #-}
  mapDim _ _ = Z

  {-# INLINE unsafeMapDim #-}
  unsafeMapDim _ _ = Z

instance (KnownNat x, Dim xs) => Dim (x:.xs) where
  {-# INLINE each #-}
  each a
    | 0 <= a && a < dimHead d = d
    | otherwise               = error "each: out of range"
   where
    d = a:.each a

  {-# INLINE rank #-}
  rank (_:.xs) = 1 + rank xs

  {-# INLINE size #-}
  size d = pdimHead d * size (pdimTail d)


  {-# INLINE next #-}
  next d@(x:.xs)
    | x < dimHead d - 1 = (x+1) :. xs
    | otherwise         = 0 :. next xs

  {-# INLINE prev #-}
  prev (x:.xs)
    | x > 0     = (x-1) :. xs
    | xs > zero = 0 :. prev xs
    | otherwise = lastDim Proxy

  {-# INLINE toIndex #-}
  toIndex   d@(x:.xs) = x + dimHead d * toIndex xs

  {-# INLINE fromIndex' #-}
  fromIndex' d ix =
    if ix > 0
    then (ix `rem` h) :. fromIndex' (pdimTail d) (ix `quot` h)
    else 0 :. zero
   where
    h = pdimHead d

  {-# INLINE correct #-}
  correct d@(x:.xs)
    | 0 <= x && x < dimHead d = x:.correct xs
    | otherwise               = error "correct: index not in range"

  {-# INLINE correctOnce #-}
  correctOnce d@(x:.xs)
    | 0 <= x && x < dimHead d = x:.xs
    | otherwise               = error "correctOnce: index not in range"

  {-# INLINE lastDim #-}
  lastDim d = (pdimHead d - 1) :. lastDim (pdimTail d)

  {-# INLINE zipDims #-}
  zipDims f d@(x:.xs) (y:.ys)
    | 0 <= r && r < dimHead d = r :. zipDims f xs ys
    | otherwise               = error "zipDims: index not in range"
   where r = f (dimHead d) x y

  {-# INLINE unsafeZipDims #-}
  unsafeZipDims f d@(x:.xs) (y:.ys) = f (dimHead d) x y :. unsafeZipDims f xs ys

  {-# INLINE mapDim #-}
  mapDim f d@(x:.xs)
    | 0 <= r && r < dimHead d = r :. mapDim f xs
    | otherwise               = error "mapDims: index not in range"
   where r = f (dimHead d) x

  {-# INLINE unsafeMapDim #-}
  unsafeMapDim f d@(x:.xs) = f (dimHead d) x :. unsafeMapDim f xs

-- | Select whether to generate an unrolled loop or just the loop at
-- compile-time.
data Mode :: * -> * where
  -- | Unroll a loop.
  Unroll   :: Ranged i => Mode i
  -- | Unroll a loop by a specific step.
  UnrollBy
    :: ( '(quot , rem) ~ QuotRem (Size dim) step, rem ~ 0
       , Range (ToPeano quot), Range (ToPeano step)
       , KnownNat step
       , Dim dim)
    => Proxy step 
    -> Proxy quot 
    -> Mode dim

  -- | Roll a loop.
  Roll   :: Dim i => Mode i

  -- | Roll a loop by a specific step.
  RollBy :: Dim i => {-# UNPACK #-} !Int -> Mode i

deriving instance Show (Mode a)

step2 :: Proxy 2
step2 = Proxy

step4 :: Proxy 4
step4 = Proxy

{-# INLINE unroll #-}
-- | You might prefer to use 'dimu'
unroll :: Ranged a => proxy a -> Mode a
unroll _ = Unroll

{-# INLINE unrollBy #-}
unrollBy 
  :: forall proxys proxy quot rem dim step.
     ( '(quot , rem) ~ QuotRem (Size dim) step, rem ~ 0
     , Range (ToPeano quot), Range (ToPeano step)
     , KnownNat step
     , Dim dim)
  => proxys step
  -> proxy dim
  -> Mode dim
unrollBy step _ = UnrollBy (Proxy :: Proxy step) Proxy

{-# INLINE roll #-}
roll :: Dim a => Proxy a -> Mode a
roll _ = Roll

{-# INLINE modeProxy #-}
modeProxy :: Mode a -> Proxy a
modeProxy _ = Proxy

{-# INLINE range #-}
-- | The range of an index
range :: Mode n -> [n]
range mode = foldrRange mode (:) []

{-# INLINE proxyOf #-}
proxyOf :: a -> Proxy a
proxyOf = const Proxy

{-# INLINE nextN #-}
nextN :: Dim a => Int -> a -> a
nextN n dim
  | n > 0     = nextN (n-1) (next dim)
  | otherwise = dim

{-# INLINE foldrRange #-}
-- | Lazy right fold over a range.
foldrRange :: Mode n -> (n -> b -> b) -> b -> b
foldrRange Unroll cons nil = sfoldrRange_ (tagPeano zero) cons nil
foldrRange Roll   cons nil = go zero
 where
  top = lastDim Proxy -- hopefully make sure this isn't recomputed
  {-# INLINE go #-}
  go !i
    | i < top   = i `cons` go (next i)
    | otherwise = cons top nil

{-# INLINE foldlRange #-}
-- | Strict left fold over a range.
foldlRange :: Mode n -> (b -> n -> b) -> b -> b
foldlRange Unroll f = sfoldlRange_ (tagPeano zero) f
foldlRange Roll   f = go zero
 where
  top = lastDim Proxy -- hopefully make sure this isn't recomputed
  go !i !acc
    | i < top   = go (next i) (f acc i)
    | otherwise = f acc i

{-# INLINE withRange #-}
-- | Compute something from a range
withRange :: Applicative m => Mode a -> (a -> m ()) -> m ()
withRange m f = foldrRange m (\d acc -> f d *> acc) (pure ())

{-# INLINE foldlRangeIndices #-}
-- | Strict left fold over the /raw/ 'Int' indices under a range
foldlRangeIndices :: Mode n -> (b -> Int -> b) -> b -> b
foldlRangeIndices m@Unroll cons = sfoldlRangeIndices_ (tagPeanoI m) cons
foldlRangeIndices m@Roll   cons = go 0
  where
    s = size m
    go !i !acc
      | i < s     = go (i+1) (cons acc i)
      | otherwise = acc

{-# INLINE foldrRangeIndices #-}
-- | Lazy right fold over the /raw/ 'Int' indices under a range
foldrRangeIndices :: Mode n -> (Int -> b -> b) -> b -> b
foldrRangeIndices m@Unroll cons nil = sfoldrRangeIndices_ (tagPeanoI m) cons nil
foldrRangeIndices m@Roll   cons nil = go 0
  where
    s = size m
    go !i
      | i < s     = i `cons` go (i+1)
      | otherwise = nil

{-# INLINE withRangeIndices #-}
-- | Compute something using the /raw/ indices under a range
withRangeIndices :: Applicative m => Mode n -> (Int -> m ()) -> m ()
withRangeIndices m f = foldrRangeIndices m (\d acc -> f d *> acc) (pure ())

{-# INLINE zipMod #-}
zipMod :: Dim n => (Int -> Int -> Int) -> n -> n -> n
zipMod f = unsafeZipDims (\h a b -> f a b `rem` h)

{-# INLINE unsafeZipDims' #-}
unsafeZipDims':: Dim n => (Int -> Int -> Int) -> n -> n -> n
unsafeZipDims' f = unsafeZipDims (\_ a b -> f a b)

{-# INLINE mapMod #-}
mapMod :: Dim n => (Int -> Int) -> n -> n
mapMod f = unsafeMapDim (\h a -> f a `rem` h)

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

instance Dim (x:.xs) => Num (x:.xs) where
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE signum #-}
  {-# INLINE abs #-}
  {-# INLINE fromInteger #-}
  (+) = zipMod (+)
  (-) = zipMod (-)
  (*) = zipMod (*)
  negate = mapMod negate
  signum = mapMod signum
  abs = mapMod abs
  fromInteger = fromIndex . fromIntegral

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

instance Dim (x:.xs) => Enum (x:.xs) where
  {-# INLINE toEnum #-}
  {-# INLINE fromEnum #-}
  {-# INLINE succ #-}
  {-# INLINE pred #-}
  {-# INLINE enumFromThen #-}
  {-# INLINE enumFromTo #-}
  {-# INLINE enumFromThenTo #-}
  toEnum               = fromIndex
  fromEnum             = toIndex

  succ d
    | d < lastDim Proxy = next d
    | otherwise         = error "succ: index within bound"

  pred d
    | d > zero  = prev d
    | otherwise = error "succ: index within bound"

  enumFrom !l
    | l < lastDim (proxyOf l) = l : enumFrom (next l)
    | otherwise               = [l]

  enumFromThen !l !j
    | l < lastDim (proxyOf l) = l : enumFromThen (zipMod (+) l j) j
    | otherwise               = [l]

  enumFromTo !l !h
    | l < h     = l : enumFromTo (next l) h
    | otherwise = [h]

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

instance (Integral xs, Dim (x:.xs), Enum (x:.xs)) => Integral (x:.xs) where
  {-# INLINE div #-}
  {-# INLINE mod #-}
  {-# INLINE quot #-}
  {-# INLINE rem #-}
  {-# INLINE quotRem #-}
  {-# INLINE divMod #-}
  {-# INLINE toInteger #-}
  div  = unsafeZipDims' div
  mod  = unsafeZipDims' mod
  quot = unsafeZipDims' quot
  rem  = unsafeZipDims' rem
  quotRem xs ys = (quot xs ys, rem xs ys)
  divMod  xs ys = (div xs ys, mod xs ys)
  toInteger = toInteger . toIndex

instance Bounded Z where
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
  minBound = Z
  maxBound = Z

-- it would be nice to write this in terms of 'Dim', but since 'Dim' requires
-- 'Bounded', the recursion confuses GHC's optimiser which leads to cruddy code
-- everywhere involving 'Dim'.
instance forall x xs. (KnownNat x, Bounded xs) => Bounded (x:.xs) where
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
  minBound = 0 :. minBound
  maxBound = (cnat (Proxy :: Proxy x)-1) :. maxBound

instance Monoid Z where
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  mempty      = Z
  mappend _ _ = Z

instance (Dim (x:.xs), Monoid xs) => Monoid (x:.xs) where
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  mempty  = zero
  mappend = (+)

instance KnownNat s => Applicative ((:.) s) where
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  {-# INLINE (*>) #-}
  {-# INLINE (<*) #-}
  pure x                    = 1:.x
  d@(ix0 :. f) <*> ix1 :. x = ((ix0*ix1) `rem` dimHead d) :. f x
  (*>)                      = (>>)
  (<*)                      = flip (>>)

instance KnownNat s => Monad ((:.) s) where
  {-# INLINE return #-}
  {-# INLINE (>>=) #-}
  return x              = 1:.x
  d@(ix0:.a) >>= f      = case f a of
    ix1 :. b -> ((ix0*ix1) `rem` dimHead d) :. b

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

dimQQ :: ExpQ -> TypeQ -> QuasiQuoter
dimQQ val ty =  QuasiQuoter
  { quoteExp  = \s -> [| $val :: $(quoteType (dimQQ val ty) s) |]
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

--------------------------------------------------------------------------------
-- Divide and conquer

data Halves
  = -- | [lower, upper)
    Split Nat -- ^ lower
          Nat -- ^ middle
          Nat -- ^ upper

  | Slices Nat Halves Nat Halves Nat
  | One   Nat
  | Two   Nat Nat
  | Three Nat Nat Nat
  | None

type family Add l k where
  Add l (Split x y z) = Split (l+x) (l+y) (l+z)

type family SplitWithDiff (ml :: Nat) (ul :: Nat) (dim :: Halves) :: k where
  SplitWithDiff 1  2  (Split l m u) = Two l m
  SplitWithDiff 1  3  (Split l m u) = Three l m (u-1)
  SplitWithDiff ml ul (Split l m u) = SplitAll
    (Slices
     l (Add l (Split1 (m-l)))
     m (Add m (Split1 (u-m)))
     u)

-- | Split a dimension into two. Useful for divide-and-conquer. Odd dimensions
-- are split with the larger half being on the left.
type family SplitAll (dim :: Halves) :: Halves where
  SplitAll (Split l m u)        = SplitWithDiff (m-l) (u-l) (Split l m u)
  SplitAll (Slices l hl m hu u) = Slices l (SplitAll hl) m (SplitAll hu) u

type family SplitsIfSizeGt1 (dim :: *) (s :: Nat) :: Halves where
  SplitsIfSizeGt1 Z       s = None
  SplitsIfSizeGt1 dim     1 = One 0
  SplitsIfSizeGt1 (x:.xs) s = SplitAll (Split1 s)

type DivideAll (dim :: *) = SplitsIfSizeGt1 dim (Size dim)

-- where...
type Split1 size      = Split2 size (Quot size 2)
type Split2 size half = Split 0 half size

data Slice a = Slice
  { start :: !Int
  , thing :: !a
  , bound :: !Int
  } deriving (Show, Eq, Ord)

class DivideAndConquer (h :: Halves) where
  dnc
    :: Monad f
    => proxy h
    -> (Int -> f t)                -- ^ first step
    -> (Slice t -> Slice t -> f t) -- ^ merge
    -> f t
          
instance forall l hl m hu u.
    ( KnownNat l, KnownNat m, KnownNat u
    , DivideAndConquer hl, DivideAndConquer hu)
    => DivideAndConquer (Slices l hl m hu u) where
  {-# INLINE dnc #-}
  dnc _ f merge = do
    l <- dnc (Proxy :: Proxy hl) f merge
    r <- dnc (Proxy :: Proxy hu) f merge
    merge
      Slice{ start = cnat (Proxy :: Proxy l)
           , thing = l
           , bound = cnat (Proxy :: Proxy m)
           }
      Slice{ start = cnat (Proxy :: Proxy m)
           , thing = r
           , bound = cnat (Proxy :: Proxy u)
           }

instance forall a. KnownNat a => DivideAndConquer (One a) where
  {-# INLINE dnc #-}
  dnc _ f _merge = f (cnat (Proxy :: Proxy a))

instance forall a b.
    (KnownNat a, KnownNat b
    ) => DivideAndConquer (Two a b) where
  {-# INLINE dnc #-}
  dnc _ f merge = do
    let a' = cnat (Proxy :: Proxy a)
        b' = cnat (Proxy :: Proxy b)
    fa <- f a'
    fb <- f b'
    merge
      Slice{ start = a'
           , thing = fa
           , bound = b'
           }
      Slice{ start = b'
           , thing = fb
           , bound = b'+1
           }

instance forall a b c.
    (KnownNat a, KnownNat b, KnownNat c
    ) => DivideAndConquer (Three a b c) where
  {-# INLINE dnc #-}
  dnc _ f merge = do
    let a' = cnat (Proxy :: Proxy a)
        b' = cnat (Proxy :: Proxy b)
        c' = cnat (Proxy :: Proxy c)
    fa <- f a'
    fb <- f b'
    fc <- f c'
    r <- merge
      Slice{ start = b'
           , thing = fb
           , bound = c'
           }
      Slice{ start = c'
           , thing = fc
           , bound = c' + 1
           }
    merge
      Slice{ start = a'
           , thing = fa
           , bound = b'
           }
      Slice{ start = b'
           , thing = r
           , bound = c' + 1
           }

data DMode (dim :: *) where
  DRoll   :: Dim dim => DMode dim
  DUnroll :: DivideAndConquer (DivideAll dim) => DMode dim

droll :: Dim dim => proxy dim -> DMode dim
droll _ = DRoll
          
dunroll :: (DivideAll dim ~ h, DivideAndConquer h) => proxy dim -> DMode dim
dunroll _ = DUnroll

{-# INLINE divideAndConquer #-}
divideAndConquer
  :: forall f dim t. Monad f
  => DMode dim
  -> (Int -> f t)
  -> (Slice t -> Slice t -> f t)
  -> f t
divideAndConquer   DUnroll f merge = dnc (Proxy :: Proxy (DivideAll dim)) f merge
divideAndConquer m@DRoll   f merge = go s0 0 s0
 where
  s0 :: Int
  s0 = size m

  go :: Int -- ^ size of current slice
     -> Int -- ^ this slice's start index
     -> Int -- ^ this slice's upper bound
     -> f t
  go n i0 iN
    | n == 1    = f i0
    | otherwise = do
        let (n2, _r) = n `quotRem` 2
            iM  = i0 + n2
        left  <- go n2     i0 iM
        right <- go (n-n2) iM iN
        merge Slice{ start = i0, thing = left  , bound = iM }
              Slice{ start = iM, thing = right , bound = iN }

-- | Compute the size of an index
type family Size (dim :: k) :: Nat where
  Size (x:.xs)  = x * Size xs
  Size Z        = 1
  Size (s::Nat) = s

type family InRange (a :: *) (b :: *) :: Bool where
  InRange Z       Z       = True
  InRange (x:.xs) (y:.ys) = x <=? y && InRange xs ys

class Range (n :: Peano) where
  sfoldrRange_ :: Dim o => Tagged n o -> (o -> b -> b) -> b -> b
  sfoldlRange_ :: Dim o => Tagged n o -> (b -> o -> b) -> b -> b

  sfoldrRangeIndices_ :: Tagged n Int -> (Int -> b -> b) -> b -> b
  sfoldlRangeIndices_ :: Tagged n Int -> (b -> Int -> b) -> b -> b

instance Range Zero where
  {-# INLINE sfoldrRange_ #-}
  {-# INLINE sfoldlRange_ #-}
  sfoldrRange_ _ _ z = z
  sfoldlRange_ _ _ z = z

  {-# INLINE sfoldrRangeIndices_ #-}
  {-# INLINE sfoldlRangeIndices_ #-}
  sfoldrRangeIndices_ _ _  z = z
  sfoldlRangeIndices_ _ _ !z = z

instance Range n => Range (Succ n) where
  {-# INLINE sfoldrRange_ #-}
  {-# INLINE sfoldlRange_ #-}
  sfoldrRange_ i f z = f (unTagged i) (sfoldrRange_ (nextTagged i) f z)
  sfoldlRange_ !i f !z = sfoldlRange_ (nextTagged i) f (f z (unTagged i))

  {-# INLINE sfoldrRangeIndices_ #-}
  {-# INLINE sfoldlRangeIndices_ #-}
  sfoldrRangeIndices_ i f z =
    f (unTagged i) (sfoldrRangeIndices_ (nextTaggedI i) f z)

  sfoldlRangeIndices_ !i f !z =
    sfoldlRangeIndices_ (nextTaggedI i) f (f z (unTagged i))

{-# INLINE nextTagged #-}
nextTagged :: Dim a => Tagged (Succ n) a -> Tagged n a
nextTagged (Tagged a) = Tagged (next a)

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

