{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans #-}

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
--import qualified Prelude
import Numeric.Extensive
import Numeric.Quaternion
import Text.PrettyPrint.Boxes


-- Want to understand the Jacobian

act :: T (Tensor H H) -> T (Hom H H)
act = extend act'
  where
    act' :: Tensor H H -> T (Hom H H)
    act' (x `Tensor` y) = hom (extend $ act'' x y)
    act'' :: H -> H -> H -> T H
    act'' x y z = return x * return z * return y

tac :: T (Hom H H) -> T (Tensor H H)
tac = inverse act

hhh :: [ T (Hom H H) ]
hhh = map return elements
