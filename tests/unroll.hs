{-# LANGUAGE QuasiQuotes, DataKinds, TypeOperators #-}
import Data.Index

myRange :: [4:.4:.Z]
myRange = srange

{-# NOINLINE test #-}
test :: a -> IO ()
test = return (return ())

{-# NOINLINE dong #-}
dong :: IO ()
dong = swithRange [dim|4 4|] test

main :: IO ()
main = do
  dong
  mapM_ print myRange
