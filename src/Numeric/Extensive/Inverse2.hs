module Numeric.Extensive.Inverse2 where

import Numeric.Extensive.Core

inverse :: (V a -> V b) -> V b -> V a
inverse = complete . loop . initialise

data OffDiagonals b = OD (b -> b -> R)

data LoopState a b
  = LS (OffDiagonals b) (V a -> V b) (V b -> V a)

initialise :: (V a -> V b) -> LoopState a b
initialise l 
  = let (b:bs) = elements
        (a:as) = elements
        e b' = if b == b' then return a else zero 
        res = extend e

    in  LS (offDiagonals l) l res

offDiagonals :: (V a -> V b) -> OffDiagonals b 
offDiagonals = undefined

complete :: LoopState a b -> V b -> V a
complete (LS _ _ out) = out

done :: LoopState a b -> Bool
done (LS diags _ _ ) = diagsAreZero diags

diagsAreZero :: OffDiagonals b -> Bool
diagsAreZero = undefined

loop :: LoopState a b -> LoopState a b
loop ls = 
    if done ls
        then ls
        else loop ls

            
