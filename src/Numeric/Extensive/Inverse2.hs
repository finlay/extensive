module Numeric.Extensive.Inverse2 where

import Numeric.Extensive.Core

inverse :: (V a -> V b) -> V b -> V a
inverse = complete . loop . initalise

data LoopState 
  = LS OffDiagonals (V a -> V b) (V b -> V a)

intialise :: (V a -> V b) -> LoopState
initalise l = LS (offDiagonals l) l (undefined)

complete :: LoopState -> V b -> V a
complete (LS _ _ out) = out

done :: LoopState -> Bool
done (LS diags _ _ ) = diagsAreZero diags

loop :: LoopState -> LoopState
loop ls = 
    if done ls
        then ls
        else loop ls

            
