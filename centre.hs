-- How to calculate the center of a Lie Algebra.

-- Borrowing ideas from deGraaf

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import qualified Prelude
import Numeric.Extensive
import Numeric.Quaternion


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
