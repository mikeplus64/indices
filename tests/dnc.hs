{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}
import Data.Index

unrolled = divideAndConquer
  (dunroll [dim|4 4|])
  (\ix    -> putStrLn $ "TIP:   " ++ show ix)
  (\s0 s1 -> putStrLn $ "MERGE: " ++ show (s0, s1))

rolled = divideAndConquer
  (droll [dim|4 4|])
  (\ix    -> putStrLn $ "TIP:   " ++ show ix)
  (\s0 s1 -> putStrLn $ "MERGE: " ++ show (s0, s1))

main :: IO ()
main = do
  putStrLn "UNROLLED"
  unrolled
  putStrLn "ROLLED"
  rolled
