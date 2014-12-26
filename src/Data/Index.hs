{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
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
  , (:.)(..), dimHead, dimTail, Z(..)
  , (.:)
    -- * Selecting whether to unroll loops
  , Mode(..), roll, unroll, modeProxy
    -- * Using ranges

    -- *** For monadic/applicative code
  , withRange
  , withRangeFrom
  , withRangeHalves
  , forRange 

    -- *** Folds
  , foldlRange
  , foldlRangeFrom
  , foldrRange
  , foldrRangeFrom

    -- ** Over 'Int' indices
    -- *** For monadic/applicative code
  , withRangeIndices
  , withRangeIndicesFrom
  , forRangeIndices

    -- *** Folds
  , foldlRangeIndices
  , foldlRangeIndicesFrom
  , foldrRangeIndices
  , foldrRangeIndicesFrom

    -- * Range types
  , Ranged
  , InRange
  , Range()
  , Peano(..)
  , ToPeano
  , Size
    -- * Utility
  , split
  , showBound
  , bounds
  , range
  , pdimHead, pdimTail
  , cnat
  , And
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
instance Rank xs ys => Rank (x:.xs) (y:.ys) where
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
  each         _ = Z

  {-# INLINE rank #-}
  rank         _ = 0

  {-# INLINE size #-}
  size         _ = 1

  {-# INLINE next #-}
  next         _ = error "next: Z"

  {-# INLINE prev #-}
  prev         _ = error "prev: Z"

  {-# INLINE toIndex #-}
  toIndex      _ = 0

  {-# INLINE fromIndex' #-}
  fromIndex' _ i | i > 0      = error "fromIndex: index too large"
                 | otherwise  = Z

  {-# INLINE correct #-}
  correct      _ = Z

  {-# INLINE correctOnce #-}
  correctOnce  _ = Z

  {-# INLINE lastDim #-}
  lastDim      _ = Z

  {-# INLINE zipDims #-}
  zipDims _ _ _  = Z

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
  fromIndex' d = \ix -> (ix `mod` h) :. fromIndex' (pdimTail d) (ix `div` h)
    where h = pdimHead d

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

deriving instance Show (Mode a)

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
foldrRange Unroll = foldrRangeFrom Unroll zero
foldrRange Roll   = foldrRangeFrom Roll   zero
-- we have to match on the Mode here to get the Dim/Ranged instances

{-# INLINE foldlRange #-}
-- | Eager left fold over a range.
foldlRange :: Mode n -> (b -> n -> b) -> b -> b
foldlRange Unroll = foldlRangeFrom Unroll zero
foldlRange Roll   = foldlRangeFrom Roll   zero

{-# INLINE foldrRangeFrom #-}
-- | Lazy right fold over a range from a given index.
foldrRangeFrom :: Mode n -> n -> (n -> b -> b) -> b -> b
foldrRangeFrom Unroll s cons nil = sfoldrRange_ (tagPeano s) cons nil
foldrRangeFrom Roll   s cons nil = go s
 where
  top = lastDim Proxy -- hopefully make sure this isn't recomputed
  {-# INLINE go #-}
  go !i
    | i < top   = i `cons` go (next i)
    | otherwise = cons top nil

{-# INLINE foldlRangeFrom #-}
-- | Lazy right fold over a range from a given index.
foldlRangeFrom :: Mode n -> n -> (b -> n -> b) -> b -> b
foldlRangeFrom Unroll s f = sfoldlRange_ (tagPeano s) f
foldlRangeFrom Roll   s f = go s
 where
  top = lastDim Proxy -- hopefully make sure this isn't recomputed
  go !i !acc
    | i < top   = go (next i) (f acc i)
    | otherwise = f acc i

{-# INLINE withRange #-}
-- | Compute something from a range
withRange :: Applicative m => Mode a -> (a -> m ()) -> m ()
withRange m f = foldrRange m (\d acc -> f d *> acc) (pure ())

{-# INLINE withRangeFrom #-}
-- | Compute something from a range, from an index.
withRangeFrom :: Applicative m => Mode a -> a -> (a -> m ()) -> m ()
withRangeFrom m z f = foldrRangeFrom m z (\d acc -> f d *> acc) (pure ())

{-# INLINE foldlRangeIndices #-}
-- | Strict left fold over the /raw/ 'Int' indices under a range
foldlRangeIndices :: Mode n -> (b -> Int -> b) -> b -> b
foldlRangeIndices m@Unroll = foldlRangeIndicesFrom m 0
foldlRangeIndices m@Roll   = foldlRangeIndicesFrom m 0

{-# INLINE foldlRangeIndicesFrom #-}
-- | Strict left fold over the /raw/ 'Int' indices under a range, from an index.
foldlRangeIndicesFrom :: Mode n -> Int -> (b -> Int -> b) -> b -> b
foldlRangeIndicesFrom m@Unroll z cons =
  sfoldlRangeIndices_ (Tagged z `asTypeOf` tagPeanoI m) cons
foldlRangeIndicesFrom m@Roll   z cons = go z
  where
    s = size m
    go !i !acc
      | i < s     = go (i+1) (cons acc i)
      | otherwise = acc

{-# INLINE foldrRangeIndices #-}
-- | Lazy right fold over the /raw/ 'Int' indices under a range
foldrRangeIndices :: Mode n -> (Int -> b -> b) -> b -> b
foldrRangeIndices m@Unroll = foldrRangeIndicesFrom m 0
foldrRangeIndices m@Roll   = foldrRangeIndicesFrom m 0

{-# INLINE foldrRangeIndicesFrom #-}
-- | Lazy right fold over the /raw/ 'Int' indices under a range, from an index.
foldrRangeIndicesFrom :: Mode n -> Int -> (Int -> b -> b) -> b -> b
foldrRangeIndicesFrom m@Unroll z cons nil =
  sfoldrRangeIndices_ (Tagged z `asTypeOf` tagPeanoI m) cons nil
foldrRangeIndicesFrom m@Roll   z cons nil = go z
  where
    s = size m
    go !i
      | i < s     = i `cons` go (i+1)
      | otherwise = nil

{-# INLINE withRangeIndices #-}
-- | Compute something using the /raw/ indices under a range
withRangeIndices :: Applicative m => Mode n -> (Int -> m ()) -> m ()
withRangeIndices m f = foldrRangeIndices m (\d acc -> f d *> acc) (pure ())

{-# INLINE withRangeIndicesFrom #-}
-- | Compute something using the /raw/ indices under a range
withRangeIndicesFrom :: Applicative m => Mode n -> Int -> (Int -> m ()) -> m ()
withRangeIndicesFrom m n f =
  foldrRangeIndicesFrom m n (\d acc -> f d *> acc) (pure ())

{-# INLINE zipMod #-}
zipMod :: Dim n => (Int -> Int -> Int) -> n -> n -> n
zipMod f = unsafeZipDims (\h a b -> f a b `mod` h)

{-# INLINE unsafeZipDims' #-}
unsafeZipDims':: Dim n => (Int -> Int -> Int) -> n -> n -> n
unsafeZipDims' f = unsafeZipDims (\_ a b -> f a b)

{-# INLINE mapMod #-}
mapMod :: Dim n => (Int -> Int) -> n -> n
mapMod f = unsafeMapDim (\h a -> f a `mod` h)

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
  pure x                    = 1:.x
  d@(ix0 :. f) <*> ix1 :. x = ((ix0*ix1) `mod` dimHead d) :. f x
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

-- | Compute the size of an index
type family Size (dim :: *) :: Nat where
  Size (x:.Z)  = x
  Size (x:.xs) = x * Size xs

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

{-# INLINE forRange #-}
-- | Similar to an imperative for loop.
forRange :: Monad m => Mode i -> (i -> m Bool) -> (i -> m ()) -> m ()
forRange mode check f = foldrRange mode r (return ())
 where
  {-# INLINE r #-}
  r ix acc = do
    ok <- check ix
    if ok
      then f ix >> acc
      else acc

{-# INLINE forRangeIndices #-}
-- | Similar to an imperative for loop.
forRangeIndices :: Monad m => Mode i -> (Int -> m Bool) -> (Int -> m ()) -> m ()
forRangeIndices mode check f = foldrRangeIndices mode r (return ())
 where
  {-# INLINE r #-}
  r ix acc = do
    ok <- check ix
    if ok
      then f ix >> acc
      else acc

type family FromPeano (n :: Peano) :: Nat where
  FromPeano (Succ n) = 1 + FromPeano n
  FromPeano Zero     = 0

type family (=?) a b then_ else_ where
  (a =? a) then_ else_ = then_
  (a =? b) then_ else_ = else_

type family HalfQuot (x :: Peano) (y :: Peano) where
  HalfQuot (Succ x) y =
    (Succ x =? y)
    (Succ x)
    ((x =? y) y (HalfQuot x (Succ y)))
  HalfQuot a b = b

type family Half (dim :: *) where
  Half (x:.Z)  = FromPeano (HalfQuot (ToPeano x) Zero) :. Z
  Half (x:.xs) = x :. Half xs
  Half Z       = Z

split
  :: Ranged (Half n)
  => Mode n
  -> (Mode (Half n), Mode n)
split Unroll = (Unroll, Unroll)
split Roll   = (Roll, Roll)

{-# INLINE withRangeHalves #-}
withRangeHalves
  :: (Dim n, Ranged (Half n), Rank (Half n) n, Applicative m)
  => Mode n
  -> (Half n -> m ()) -- ^ Function to run on first half
  -> (n -> m ())      -- ^ Function to run on second half
  -> m ()
withRangeHalves s fl fr =
  withRange l fl
  *> withRangeFrom r (next (setBound (maxBound `asModeOf` l))) fr
 where (l, r) = split s

asModeOf :: a -> proxy a -> a
asModeOf a _ = a
