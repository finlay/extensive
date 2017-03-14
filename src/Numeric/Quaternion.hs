{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Numeric.Quaternion where

import Numeric.Algebra hiding (zero)
import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)

import Numeric.Extensive


data H = E | I | J | K deriving (Eq, Ord)
instance FiniteSet H where elements = [ E, I, J, K ]
instance Show H where
    show E = "e" ; show I = "i"
    show J = "j" ; show K = "k"

e, i, j, k :: T H
[e,i,j,k] = map return elements

mu :: T (Tensor H H) -> T H
mu = extend mu'
  where
    mu' :: Tensor H H -> T H
    mu' (E `Tensor` b) = return b
    mu' (b `Tensor` E) = return b
    mu' (I `Tensor` J) = k
    mu' (J `Tensor` K) = i
    mu' (K `Tensor` I) = j
    mu' (J `Tensor` I) = minus k
    mu' (K `Tensor` J) = minus i
    mu' (I `Tensor` K) = minus j
    mu' (I `Tensor` I) = minus e
    mu' (J `Tensor` J) = minus e
    mu' (K `Tensor` K) = minus e

instance Multiplicative (T H) where
    (*) x' y' = mu (x' `tensor` y')

-- Now lets make Tensor H H an algebra
instance Multiplicative (T (Tensor H H)) where
    (*) x' y' = extend muHH (x' `tensor` y')
            where 
                muHH  (Tensor (Tensor xe ye) (Tensor xe' ye')) 
                    = ((return xe) * (return xe')) `tensor` ((return ye') * (return ye))

comm :: (Multiplicative r, Group r)
     => r -> r -> r
comm a b = a * b - b * a

ehh :: [T (Tensor H H)]
ehh = map return elements

-- Create a new more convenient basis for H Tensor H
-- Need to give names, and elements
-- Then, expand arbitary elements in the new basis
data Tau = Sym H H | Skew H H deriving (Ord, Eq)
instance Order Tau where
    order a b = Just (compare a b)
instance Show Tau where
    show (Sym  a b) = show a ++ " \x2228 " ++ show b
    show (Skew a b) = show a ++ " \x2227 " ++ show b

sym0, sym1, sym2, ske1, ske2 :: [Tau]
sym0 = [ Sym  a a | a <- [E,I,J,K]]
sym1 = [ Sym  E a | a <- [I,J,K]]
sym2 = [ Sym  J K , Sym  K I, Sym  I J]
ske1 = [ Skew E a | a <- [I,J,K]]
ske2 = [ Skew J K , Skew K I, Skew I J]

instance FiniteSet Tau where
    elements =  sym0 ++ sym1 ++ sym2 ++ ske1 ++ ske2

instance Multiplicative (T Tau) where
    (*) x' y' = injectTauInv ((injectTau x') * (injectTau y'))

tau :: [T Tau]
tau = map return elements

injectTau :: T Tau -> T (Tensor H H)
injectTau = extend injectTau'
  where
    injectTau' (Sym  xe ye) = let x' = return xe 
                                  y' = return ye
                              in  scale 0.5 (x' `tensor` y' + y' `tensor` x')
    injectTau' (Skew xe ye) = let x' = return xe 
                                  y' = return ye
                              in  scale 0.5 (x' `tensor` y' - y' `tensor` x')

injectTauInv' :: T (Tensor H H) -> T Tau
injectTauInv' = extend injectTauInv'
  where
    injectTauInv' (E `Tensor` E) = return (Sym E E) 
    injectTauInv' (I `Tensor` I) = return (Sym I I)
    injectTauInv' (J `Tensor` J) = return (Sym J J)
    injectTauInv' (K `Tensor` K) = return (Sym K K)
    injectTauInv' (E `Tensor` I) = (return (Sym E I)) + (return (Skew E I))
    injectTauInv' (I `Tensor` E) = (return (Sym E I)) - (return (Skew E I))
    injectTauInv' (E `Tensor` J) = (return (Sym E J)) + (return (Skew E J))
    injectTauInv' (J `Tensor` E) = (return (Sym E J)) - (return (Skew E J))
    injectTauInv' (E `Tensor` K) = (return (Sym E K)) + (return (Skew E K))
    injectTauInv' (K `Tensor` E) = (return (Sym E K)) - (return (Skew E K))
    injectTauInv' (I `Tensor` J) = (return (Sym I J)) + (return (Skew I J))
    injectTauInv' (J `Tensor` I) = (return (Sym I J)) - (return (Skew I J))
    injectTauInv' (J `Tensor` K) = (return (Sym J K)) + (return (Skew J K))
    injectTauInv' (K `Tensor` J) = (return (Sym J K)) - (return (Skew J K))
    injectTauInv' (K `Tensor` I) = (return (Sym K I)) + (return (Skew K I))
    injectTauInv' (I `Tensor` K) = (return (Sym K I)) - (return (Skew K I))

injectTauInv :: T (Tensor H H) -> T Tau
injectTauInv = inverse injectTau

-- Killing form
killing :: (Multiplicative (T a), FiniteSet a, Eq a) => T a -> T a -> R
killing x' y' = trace (ad x' . ad y')
  where
    ad = comm 
    trace f = sum $ map (diag f) elements
    coef (T v) = v . delta
    diag f e' = coef (f (return e')) e'

-- Construct as Lie algebra

-- Define a new set of generators, based on a few basics
data SO3 = X | Y | Z deriving (Eq, Ord)
instance FiniteSet SO3 where elements = [X, Y, Z]
instance Show SO3
    where show X = "x"
          show Y = "y"
          show Z = "z"



x, y, z :: T SO3
[x, y, z] = map return elements

instance Multiplicative (T SO3) where
    (*) x' y' = mmu (x' `tensor` y')
      where 
        mmu :: T (Tensor SO3 SO3) -> T SO3
        mmu = extend mmu'
        mmu' (X `Tensor` X) = zero
        mmu' (X `Tensor` Y) = return Z
        mmu' (X `Tensor` Z) = minus (return Y)
        mmu' (Y `Tensor` X) = minus (return Z)
        mmu' (Y `Tensor` Y) = zero
        mmu' (Y `Tensor` Z) = return X
        mmu' (Z `Tensor` X) = return Y
        mmu' (Z `Tensor` Y) = minus (return X)
        mmu' (Z `Tensor` Z) = zero

so3 :: T SO3 -> T (Tau)
so3 = extend so3'
  where 
    so3' X = return $ Skew E I
    so3' Y = return $ Skew E J
    so3' Z = return $ Skew E K

