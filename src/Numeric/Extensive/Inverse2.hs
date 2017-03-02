module Numeric.Extensive.Inverse2 where

import Numeric.Extensive.Core

inverse 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (T a -> T b) -> T b -> T a
inverse = complete . loop . initialise

data LoopState a b
  = LS (T a -> T b) (T b -> T a)

initialise 
  :: (Eq b, FiniteSet a, FiniteSet b) 
  => (T a -> T b) -> LoopState a b
initialise l 
  = let (b:bs) = elements
        (a:as) = elements
        e b' = if b == b' then return a else zero 
        res = extend e
    in  LS l res

complete :: LoopState a b -> T b -> T a
complete (LS _ out) = out

-- Question about how to stop the loop
-- Lets try to skip this bit
diagsAreZero :: (T b -> T b) -> Maybe (R, a,b)
diagsAreZero e
  = undefined

loop :: LoopState a b -> LoopState a b
loop (LS l r)
  = undefined
--   case diagsAreZero (l . r) of
--     Nothing -> LS l r
--     Just (c, x, y) 
--       -> let r' = r . diag c x y
--          in  loop (LS l r')

            
