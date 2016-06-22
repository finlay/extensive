module Numeric.Extensive.Inverse2 where

import Numeric.Extensive.Core

inverse 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (V a -> V b) -> V b -> V a
inverse = complete . loop . initialise

data LoopState a b
  = LS (V a -> V b) (V b -> V a)

initialise 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (V a -> V b) -> LoopState a b
initialise l 
  = let (b:bs) = elements
        (a:as) = elements
        e b' = if b == b' then return a else zero 
        res = extend e
    in  LS l res

complete :: LoopState a b -> V b -> V a
complete (LS _ out) = out

diagsAreZero :: (V b -> V b) -> Maybe (R, a,b)
diagsAreZero e
  = undefined

loop :: LoopState a b -> LoopState a b
loop (LS l r) = 
  case diagsAreZero (l . r) of
    Nothing -> LS l r
    Just (c, x, y) 
      -> let r' = r . diag c x y
         in  loop (LS l r')

            
