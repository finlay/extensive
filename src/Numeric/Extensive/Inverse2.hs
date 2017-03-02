module Numeric.Extensive.Inverse2 where

import Numeric.Extensive.Core

inverse 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (T a -> T b) -> T b -> T a
inverse = complete . loop . initialise

data LoopState a b
  = LS (V a -> V b) (V a -> V a)

initialise 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (T a -> T b) -> LoopState a b
initialise l 
  = LS l (transpose l . l)

complete :: LoopState a b -> T b -> T a
complete (LS _ out) = out

-- Question about how to stop the loop
-- Lets try to skip this bit
diagsAreZero :: (T b -> T b) -> Maybe (R, a,b)
diagsAreZero e
  = undefined

loop :: LoopState a b -> LoopState a b
loop (LS l r) = undefined

            
