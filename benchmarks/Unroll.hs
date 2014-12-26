{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}
import Data.Index
import Foreign
import Foreign.Marshal.Array

{-# NOINLINE thing #-}
thing :: a -> IO ()
thing _ = return ()

-- check ghc core
main :: IO ()
main = do
  let s = [dim|4 4|]
  {-
    case {__pkg_ccall base malloc Word#
                     -> State# RealWorld -> (# State# RealWorld, Addr# #)}
       (__word 128) eta
    of _ { (# ds, ds1 #) ->
  -}
  ptr <- mallocArray (size s)

  {-
    case hPutStr2 stdout lvl15 True ds of _ { (# ipv, ipv1 #) ->
    case hPutStr2 stdout lvl14 True ipv of _ { (# ipv2, ipv3 #) ->
    case hPutStr2 stdout lvl13 True ipv2 of _ { (# ipv4, ipv5 #) ->
    case hPutStr2 stdout lvl12 True ipv4 of _ { (# ipv6, ipv7 #) ->
    case hPutStr2 stdout lvl11 True ipv6 of _ { (# ipv8, ipv9 #) ->
    case hPutStr2 stdout lvl10 True ipv8 of _ { (# ipv10, ipv11 #) ->
    case hPutStr2 stdout lvl9 True ipv10 of _ { (# ipv12, ipv13 #) ->
    case hPutStr2 stdout lvl8 True ipv12 of _ { (# ipv14, ipv15 #) ->
    case hPutStr2 stdout lvl7 True ipv14 of _ { (# ipv16, ipv17 #) ->
    case hPutStr2 stdout lvl6 True ipv16 of _ { (# ipv18, ipv19 #) ->
    case hPutStr2 stdout lvl5 True ipv18 of _ { (# ipv20, ipv21 #) ->
    case hPutStr2 stdout lvl4 True ipv20 of _ { (# ipv22, ipv23 #) ->
    case hPutStr2 stdout lvl3 True ipv22 of _ { (# ipv24, ipv25 #) ->
    case hPutStr2 stdout lvl2 True ipv24 of _ { (# ipv26, ipv27 #) ->
    case hPutStr2 stdout lvl1 True ipv26 of _ { (# ipv28, ipv29 #) ->
    case hPutStr2 stdout lvl True ipv28 of _ { (# ipv30, ipv31 #) ->
  -}
  withRange (unroll s) (print . toIndex)


  {-
    case writeIntOffAddr# ds1 15 15 ipv30 of s2 { __DEFAULT ->
    case writeIntOffAddr# ds1 14 14 s2 of s1 { __DEFAULT ->
    case writeIntOffAddr# ds1 13 13 s1 of s3 { __DEFAULT ->
    case writeIntOffAddr# ds1 12 12 s3 of s4 { __DEFAULT ->
    case writeIntOffAddr# ds1 11 11 s4 of s5 { __DEFAULT ->
    case writeIntOffAddr# ds1 10 10 s5 of s6 { __DEFAULT ->
    case writeIntOffAddr# ds1 9 9 s6 of s7 { __DEFAULT ->
    case writeIntOffAddr# ds1 8 8 s7 of s8 { __DEFAULT ->
    case writeIntOffAddr# ds1 7 7 s8 of s9 { __DEFAULT ->
    case writeIntOffAddr# ds1 6 6 s9 of s10 { __DEFAULT ->
    case writeIntOffAddr# ds1 5 5 s10 of s11 { __DEFAULT ->
    case writeIntOffAddr# ds1 4 4 s11 of s12 { __DEFAULT ->
    case writeIntOffAddr# ds1 3 3 s12 of s13 { __DEFAULT ->
    case writeIntOffAddr# ds1 2 2 s13 of s14 { __DEFAULT ->
    case writeIntOffAddr# ds1 1 1 s14 of s15 { __DEFAULT ->
    case writeIntOffAddr# ds1 0 0 s15 of s16 { __DEFAULT ->
  -}
  withRangeIndices (unroll s) $ \ix ->
    pokeElemOff ptr ix ix

  {-
    letrec {
      $wa :: Int# -> State# RealWorld -> (# State# RealWorld, () #)
      $wa =
        \ (ww :: Int#) (w :: State# RealWorld) ->
          case tagToEnum# (<# ww 16) of _ {
            False -> (# w, () #);
            True ->
              case $wa (+# ww 1) w of _ { (# ipv32, ipv33 #) ->
              case writeIntOffAddr# ds1 ww ww ipv32 of s17 { __DEFAULT ->
              (# s17, () #)
              }
              }
          }; } in
    $wa 0 s16
  -}
  withRangeIndices (roll s) $ \ix ->
    pokeElemOff ptr ix ix
