{-# LANGUAGE DataKinds #-}
-- How to calculate the center of a Lie Algebra.

-- Borrowing ideas from deGraaf

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
--import qualified Prelude
import Numeric.Extensive
import Numeric.Quaternion

--  hom $ l . kernel l == zero
kernel :: (T a -> T b) -> T (N n) -> T a
kernel _l = undefined

test :: T H -> T H
test = extend t'
  where 
    t' E = return E
    t' I = return I
    t' J = zero
    t' K = zero

kernel_test :: T (N 2) -> T H
kernel_test = extend t'
  where 
    t' (N 1) = return J
    t' (N 2) = return K

-- If we have an algebra, how do we calculate the centre ?
-- And what does the answer look like ?
-- 
-- centre :: (FiniteSet a, FiniteSet b) 
--        => T a -> ( T b, T b -> T a)
-- centre _
--   = let bas :: [a]
--         bas = elements 
--     in  undefined
-- 
main :: IO()
main = putStrLn "Hi"
