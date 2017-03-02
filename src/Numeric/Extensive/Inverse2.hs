module Numeric.Extensive.Inverse2 where

import Numeric.Extensive.Core

inverse 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (V a -> V b) -> V b -> V a
inverse = complete . loop . initialise

data LoopState a b
  = LS (V a -> V b) (V a -> V a)

initialise 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (V a -> V b) -> LoopState a b
initialise l 
  = LS l (transpose l . l)

complete :: LoopState a b -> V b -> V a
complete (LS _ out) = out

diagsAreZero :: (V b -> V b) -> Maybe (R, a,b)
diagsAreZero e
  = undefined

loop :: LoopState a b -> LoopState a b
loop (LS l r) = undefined

            
