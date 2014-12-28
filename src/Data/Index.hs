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
  , Mode(..), roll, unroll, modeProxy
    -- * Using ranges
  , foldlRange
  , foldrRange
  , withRange
    -- ** Over 'Int' indices
  , foldlRangeIndices
  , foldrRangeIndices
  , withRangeIndices
    -- * Algorithms
  , DivideAndConquer
  , divideAndConquer
  , DMode(..)
  , dunroll, droll
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
  , Divide
  , Halves'(..)
  , Halves
  , Half
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
import           Data.Tagged
import           Data.Traversable          (Traversable)
import           GHC.Generics
import           GHC.TypeLits
import           Language.Haskell.TH       hiding (Range (..))
import           Language.Haskell.TH.Quote
-- import Debug.Trace

-- | Index constructor, analogous to ':'
--
-- The 'Applicative' and 'Monad' instances multiply in their bind operations,
-- and their 'return'/'pure' creates an index whose first dimension is 1.
data a :. b = {-# UNPACK #-} !Int :. !b
  deriving (Show,Read,Eq,Ord,Generic,Functor,Foldable,Traversable)

infixr 9 :.

{-# INLINE (.:) #-}
-- | Construct an index. This ensures that all constructed indices are valid by
-- taking the remainder. Negative indices go to 0.
(.:) :: forall x xs. KnownNat x => Int -> xs -> x:.xs
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
  Unroll :: Ranged i => Mode i
  Roll   :: Dim i => Mode i

-- | You might prefer to use 'dimu'
{-# INLINE unroll #-}
unroll :: Ranged a => Proxy a -> Mode a
unroll _ = Unroll

-- | You might prefer to use 'dimr'
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
-- | Lazy right fold over a range.
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

-- | Peano numbers
data Peano = Zero | Succ Peano

-- | Convert a 'Nat' to a type-level 'Peano'
type family ToPeano (n :: Nat) :: Peano where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n-1))

type family FromPeano (n :: Peano) :: Nat where
  FromPeano (Succ n) = 1 + FromPeano n
  FromPeano Zero = 0

type family (=?) a b then_ else_ where
  (a =? a) then_ else_ = then_
  (a =? b) then_ else_ = else_

type family Half' (phx :: Nat) (hx :: Nat) (tx :: Nat) (x :: Nat) where
  Half' hx' hx 0 x = hx' - 2 -- even
  Half' hx' hx 1 x = hx' - 2 -- odd
  Half' phx hx t x = Half' (phx+1) (phx*2) (x - hx) x

type family Half (x :: Nat) where
  Half 0 = 0
  Half 1 = 0
  Half n = Half' 0 0 2 n

data Halves' a
  = -- | [lower, upper)
    Slice !a -- ^ lower
          !a -- ^ middle
          !a -- ^ upper
  | Tip2 !a !a
  | Tip1 !a
  | Slices !a (Halves' a) !a (Halves' a) !a
 deriving (Show, Eq, Ord)

type Halves = Halves' Int

type family Add l k where
  Add l (Slice x y z) = Slice (l+x) (l+y) (l+z)

-- | Split a dimension into two. Useful for divide-and-conquer. Odd dimensions
-- are split with the larger half being on the left.
type family SplitOnce (dim :: k) :: Halves' Nat where
  SplitOnce (dim :: *)    = Split1 (Size dim)
  SplitOnce (Slice l m u) =
    Slices
      l (Add l (Split1 (m-l)))
      m (Add m (Split1 (u-m)))
      u

  SplitOnce (Slices l hl m hu u) =
    Slices l (SplitOnce hl) m (SplitOnce hu) u

-- where...
type Split1 size      = Split2 size (Half size)
type Split2 size half = Slice 0 half size


type Splits'0 h = Splits' h (SliceDiff h)

type family Splits' (dim :: k) (diff :: Peano) :: Halves' Nat where
  Splits' (h :: Halves' Nat) (Succ Zero) = Tips h
  Splits' (h :: k)           diff        = Splits'0 (SplitOnce h)

type family Tips (halves :: Halves' Nat) :: Halves' Nat where
  Tips (Slice  a b c)       = ((c-a) =? 2) (Tip2 a c) (Slice a b c)
  Tips (Slices l hl m hu u) = Slices l (Tips hl) m (Tips hu) u

type Divide (dim :: *) = Splits' dim Zero

data DMode (h :: Maybe (Halves' Nat)) dim where
  DRoll   :: Dim dim => DMode Nothing dim
  DUnroll :: (Divide dim ~ h, DivideAndConquer h) => DMode (Just h) dim

droll :: Dim dim => proxy dim -> DMode Nothing dim
droll _ = DRoll
          
dunroll :: (Divide dim ~ h, DivideAndConquer h) => proxy dim -> DMode (Just h) dim
dunroll _ = DUnroll

type family UnJust m where
  UnJust (Just a) = a

divideAndConquer
  :: forall dim h t. DMode h dim
  -> (Int -> t)
  -> (Int -> Int -> t)
  -> (Int -> t -> Int -> t -> Int -> t)
  -> t
divideAndConquer   DUnroll f1 f2 conq = divide' (Proxy :: Proxy (UnJust h)) f2 conq
divideAndConquer r@DRoll   f1 f2 conq = split {- 0 -} s0 0 s0
 where
  s0 = size r
  split -- n
        s -- ^ size of this partition
        l -- ^ this partition's start index
        u -- ^ this partition's end index (non-inclusive)
 -- | trace (replicate (2*n) ' ' ++ show s ++ show (l,u)) False = error "what"
    | s <= 2    =
        if s == 1
        then f1 l
        else f2 l (u-1)
    | otherwise =
        let (s', r') = s `quotRem` 2
            m        = l + s'
        in conq
           l (split {- (n+1) -} s'     l (l+s'))
           m (split {- (n+1) -} (s-s') m (m+s'+r'))
           u
  
type family SliceDiff (s :: Halves' Nat) :: Peano where
  SliceDiff (Slice  a b c)       = ToPeano (b - a)
  SliceDiff (Slices l hl m hr r) = SliceDiff hl

class DivideAndConquer (h :: Halves' Nat) where
  divide'
    :: Proxy h
    -> (Int -> Int -> t) -- ^ divide
    -> (Int -> t ->
        Int -> t ->
        Int -> t) -- ^ conquer
    -> t
          
instance forall l hl m hu u.
    ( KnownNat l, KnownNat m, KnownNat u
    , DivideAndConquer hl, DivideAndConquer hu)
    => DivideAndConquer (Slices l hl m hu u) where
  {-# INLINE divide' #-}
  divide' _ f combine =
    combine
      (cnat    (Proxy :: Proxy l))
      (divide' (Proxy :: Proxy hl) f combine)
      (cnat    (Proxy :: Proxy m))
      (divide' (Proxy :: Proxy hu) f combine)
      (cnat    (Proxy :: Proxy u))

instance forall a b. (KnownNat a, KnownNat b)
         => DivideAndConquer (Tip2 a b) where
  {-# INLINE divide' #-}
  divide' _ f _conquer =
    f (cnat (Proxy :: Proxy a)) (cnat (Proxy :: Proxy b))

instance forall l m u. (KnownNat l, KnownNat m, KnownNat u)
         => DivideAndConquer (Slice l m u) where
  divide' _ f _combine = f
    (cnat (Proxy :: Proxy l))
    (cnat (Proxy :: Proxy u))

    -- Slices (Add l (Split0 (m-l))) (Add m (Split0 (u-m)))

{-
{-# INLINE splitIndices #-}
-- | Split an index in two by halving it.
splitIndices
  :: forall dim l r. (Split dim ~ '(l, r), KnownNat l, KnownNat r)
  => Proxy dim
  -> (Int, Int)
splitIndices _ = (cnat (Proxy :: Proxy l), cnat (Proxy :: Proxy r))

{-# INLINE split #-}
-- | Split an index in two by halving it.
split
  :: forall dim l r. (Split dim ~ '(l, r), KnownNat l, KnownNat r)
  => Proxy dim
  -> Proxy '(l, r)
split _ = Proxy
-}

-- | Compute the size of an index
type family Size (dim :: *) :: Nat where
  Size (x:.xs) = x * Size xs
  Size Z       = 1

type family InRange (a :: *) (b :: *) :: Bool where
  InRange Z       Z       = True
  InRange (x:.xs) (y:.ys) = And (x <=? y) (InRange xs ys)

type family And (a :: Bool) (b :: Bool) :: Bool where
  And True True = True
  And a    b    = False

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

