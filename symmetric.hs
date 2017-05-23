{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

import Numeric.Algebra hiding (zero)
import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import System.Random
import qualified Test.QuickCheck as QC
import qualified Prelude
import Numeric.Extensive
import Text.PrettyPrint.Boxes


data Sym3 
    = E | Tau | Sigma | SigmaSigma | TauSigma | TauSigmaSigma
    deriving (Eq, Ord)
instance FiniteSet Sym3 where 
    elements = [E,Tau,Sigma,SigmaSigma,TauSigma,TauSigmaSigma]
instance Show Sym3 where
    show E = "1"
    show Tau = "τ"
    show Sigma = "σ"
    show SigmaSigma = "σσ"
    show TauSigma = "τσ"
    show TauSigmaSigma = "τσσ"

mu :: T (Tensor Sym3 Sym3) -> T Sym3
mu = extend mu'
  where
    mu' :: Tensor Sym3 Sym3 -> T Sym3
    mu' (Tau           `Tensor` Tau)            = return E
    mu' (Tau           `Tensor` Sigma)          = return TauSigma
    mu' (Tau           `Tensor` TauSigma)       = return Sigma
    mu' (Tau           `Tensor` SigmaSigma)     = return TauSigmaSigma
    mu' (Tau           `Tensor` TauSigmaSigma)  = return SigmaSigma
    mu' (Sigma         `Tensor` Tau)            = return TauSigmaSigma
    mu' (Sigma         `Tensor` Sigma)          = return SigmaSigma
    mu' (Sigma         `Tensor` TauSigma)       = return Tau
    mu' (Sigma         `Tensor` SigmaSigma)     = return E
    mu' (Sigma         `Tensor` TauSigmaSigma)  = return TauSigma
    mu' (TauSigma      `Tensor` Tau)            = return SigmaSigma
    mu' (TauSigma      `Tensor` Sigma)          = return TauSigmaSigma
    mu' (TauSigma      `Tensor` TauSigma)       = return E
    mu' (TauSigma      `Tensor` SigmaSigma)     = return Tau
    mu' (TauSigma      `Tensor` TauSigmaSigma)  = return Sigma
    mu' (SigmaSigma    `Tensor` Tau)            = return TauSigma
    mu' (SigmaSigma    `Tensor` Sigma)          = return E
    mu' (SigmaSigma    `Tensor` TauSigma)       = return TauSigmaSigma
    mu' (SigmaSigma    `Tensor` SigmaSigma)     = return Sigma
    mu' (SigmaSigma    `Tensor` TauSigmaSigma)  = return Tau
    mu' (TauSigmaSigma `Tensor` Tau)            = return Sigma
    mu' (TauSigmaSigma `Tensor` Sigma)          = return Tau
    mu' (TauSigmaSigma `Tensor` TauSigma)       = return SigmaSigma
    mu' (TauSigmaSigma `Tensor` SigmaSigma)     = return TauSigma
    mu' (TauSigmaSigma `Tensor` TauSigmaSigma)  = return E
    mu' (E             `Tensor` x)              = return x
    mu' (x             `Tensor` E)              = return x

instance Multiplicative (T Sym3) where
    (*) x' y' = mu (x' `tensor` y')


o :: T Sym3
o = return E

s,σ :: T Sym3
σ = return Sigma
s = σ

t,τ :: T Sym3
τ = return Tau
t = τ

bss :: [ T Sym3 ]
bss = [ return x | x <- elements ]

pi0,pi1,pi2 :: T Sym3
pi0 = scale (1.0 Prelude./6) $ ( o + t) * ( o + s + s*s )
pi1 = scale (1.0 Prelude./6) $ ( o - t) * ( o + s + s*s )
pi2 = scale (1.0 Prelude./3) $ ( scale 2 o - s - s*s )

inbasis :: Box
inbasis = 
  let row = vsep 2 right
      header = row (text "": [ text (show x) | x <- bss ])
      rest = [ row ( text (show x) 
                   : [ text  (show (x * y)) | y <- bss ] ) 
             | x <- bss ]
  in  hsep 4 bottom ( header : rest )


proj  :: T Sym3 ->  [ T Sym3 ] ->  Box
proj pr bss = 
  let col = vsep 2 right
      xs = col [text (show x) | x <- bss ]
      e1xs = col [text (show (pr * x)) | x <- bss ]
  in  hsep 4 bottom [ xs, e1xs ]

tbl  :: [ T Sym3 ] -> [ T Sym3 ] ->  Box
tbl xs ys = 
  let col = vsep 2 right
      lftcol = col (text "" : [text (show x) | x <- xs ])
      prods = [ col (text (show y) 
                    : [ text (show (x * y)) | x <- xs ] 
                    ) | y <- ys ]
  in  hsep 4 bottom ( lftcol : prods )




-- Make a random matrix
randomElement :: (FiniteSet a) => Double -> IO (T a)
randomElement p 
  = foldl1 plus <$> (mapM sce =<< els)
  where
      bernoulli :: (Num a, Ord a, Random a) => a -> IO Bool
      bernoulli p = (Prelude.> p) <$> randomRIO (0, 1)
      choose :: Double -> a -> a -> IO a
      choose p a b =
          do r <- bernoulli p
             return $ if r then a else b
      els :: FiniteSet a => IO [T a]
      els = mapM (choose p zero . return) elements
      sce :: T a -> IO (T a)
      sce b = fmap (\x -> scale x b) (QC.generate QC.arbitrary )

randomMatrix 
    :: (FiniteSet a, Eq a, FiniteSet b, Eq b) 
    => Double -> IO (T a -> T b)
randomMatrix p = apply <$> randomElement p



main :: IO()
main = putStrLn "Hello!"
