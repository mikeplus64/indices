{-# LANGUAGE QuasiQuotes, TemplateHaskell, Rank2Types, TypeOperators, DataKinds, KindSignatures, FlexibleInstances #-}
module QCUtil where
import GHC.TypeLits
import Test.QuickCheck
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Data.IORef
import System.Random
import Control.Monad
import Data.Index

properties :: IORef [Name]
properties = unsafePerformIO (newIORef [])

checkProps :: ExpQ
checkProps = do
    props <- runIO (readIORef properties)
    [| verboseCheck $(manyLabeledTests (map toLabelVar props)) |]
  where
    toLabelVar p = (lift (nameBase p), varE p)

manyLabeledTests :: [(ExpQ,ExpQ)] -> ExpQ
manyLabeledTests xs = foldr (\(n,x) y -> [| verbose (label $n $x) .&. $y |]) [| True |] xs


manyTests :: [ExpQ] -> ExpQ
manyTests xs = foldr (\x y -> [| $x .&. $y |]) [| True |] xs

testing :: Int -> String -> Name -> Q [Dec]
testing n base fun = do
    runIO (modifyIORef properties (pname:))
    decl . manyTests . map randomDims . replicate n $ fun
  where
    pname   = mkName base
    decl xs =
      sequence [ sigD pname [t| Property |]
               , valD (varP pname) (normalB xs) []
               ]
  
  {-
  forM (map (\i -> base ++ show i) [1..n]) $ \name ->
    let propName = mkName name
    in valD (varP propName)
            (normalB (quickCheckDims f))
            []
  -}

randomNat :: TypeQ
randomNat = do
  i <- runIO (randomRIO (0,maxBound :: Int))
  litT (numTyLit (fromIntegral i))

someNats :: Q [TypeQ]
someNats = do
  i  <- runIO (randomRIO (0,10))
  map return `fmap` replicateM i randomNat

natsToDim :: [TypeQ] -> TypeQ
natsToDim []     = [t| Z |]
natsToDim (x:xs) = [t| $x :. $(natsToDim xs) |]

randomTyDim :: TypeQ
randomTyDim = natsToDim =<< someNats

randomDims :: Name -> ExpQ
randomDims = flip setVars randomTyDim

setVars :: Name -> TypeQ -> ExpQ
setVars t to = do
  ty0 <- fmap infoType (reify t)
  (polys, _, ty) <- deconstructType ty0
  case polys of
    [] -> return (VarE t)
    _ -> do
      integer <- to
      ty' <- monomorphise integer ty
      return (SigE (VarE t) ty')

monomorphise :: Type -> Type -> TypeQ
monomorphise mono ty@(VarT n) = return mono
monomorphise mono (AppT t1 t2) = liftM2 AppT (monomorphise mono t1) (monomorphise mono t2)
monomorphise mono ty@(ForallT _ _ _) = error "Higher-ranked type"
monomorphise mono ty = return ty

infoType :: Info -> Type
infoType (ClassOpI _ ty _ _) = ty
infoType (DataConI _ ty _ _) = ty
infoType (VarI _ ty _ _) = ty

deconstructType :: Type -> Q ([Name], Cxt, Type)
deconstructType ty0@(ForallT xs ctx ty) = do
  let plain (PlainTV _) = True
      plain _ = False
  unless (all plain xs) $ error "Higher-kinded type variables in type"
  return (map (\(PlainTV x) -> x) xs, ctx, ty)
deconstructType ty = return ([], [], ty)
