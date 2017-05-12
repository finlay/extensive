{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

import Numeric.Algebra hiding (zero)
import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import Numeric.Extensive


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


res :: [ (T Sym3, T Sym3) ]
res = [ (return x, return y)
      | x <- elements, y <- elements ] 

e1,f1,g1 :: T Sym3
e1 = scale (1.0 Prelude./6) $ ( o + t) * ( o + s + s*s )
f1 = scale (1.0 Prelude./6) $ ( o - t) * ( o + s + s*s )
g1 = scale (1.0 Prelude./3) $ ( scale 2 o - s - s*s )

main :: IO()
main = putStrLn "Hello!"
