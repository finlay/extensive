{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Numeric.Quaternion where

import Numeric.Algebra hiding (zero)
import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)

import Numeric.Extensive


data H = E | I | J | K deriving (Eq, Ord)
instance FiniteSet H where elements = [ E, I, J, K ]
instance Show H where
    show E = "e" ; show I = "i"
    show J = "j" ; show K = "k"
instance Order H where
    order E I = Just LT;   order I E = Just GT
    order E J = Just LT;   order J E = Just GT
    order E K = Just LT;   order K E = Just GT
    order I J = Just LT;   order J I = Just GT
    order I K = Just LT;   order K I = Just GT
    order J K = Just LT;   order K J = Just GT
    order _ _ = Just EQ

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

injectTauInv :: T (Tensor H H) -> T Tau
injectTauInv = force $ inverse injectTau

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
    so3' X = scale (-1) $ return $ Skew E I
    so3' Y = scale (-1) $ return $ Skew E J
    so3' Z = scale (-1) $ return $ Skew E K


-- Check if a function is a homomorphism
isHomomorphism
    :: ( FiniteSet a, FiniteSet b, Eq b)
    => (T a -> T a -> T a)
    -> (T b -> T b -> T b)
    -> (T a -> T b) -> [(T a,T a)]
isHomomorphism m1 m2 l
  = [ (x1,y1) | x1 <- basis
              , y1 <- basis
              , (l x1) `m2` (l y1) /= l (x1 `m1` y1) ]


