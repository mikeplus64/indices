{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric, GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Index.Lens where
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Index
import Control.Lens

data Full a
data Raw a
data Ix s where
  Full :: !s -> Ix (Full s)
  Raw  :: {-# UNPACK #-} !Int -> Ix (Raw s)

fullToRaw :: Dim s => Ix (Full s) -> Ix (Raw s)
fullToRaw (Full s) = Raw (toIndex s)

rawToFull :: Dim s => Ix (Raw s) -> Ix (Full s)
rawToFull (Raw s)  = Full (fromIndex s)

ixraw :: Dim s => Iso' (Ix (Full s)) (Ix (Raw s))
ixraw = iso fullToRaw rawToFull
