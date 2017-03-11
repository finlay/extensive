{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Numeric.Extensive.Inverse where

import Debug.Trace
import Numeric.Extensive.Core

--   \begin{align*}
--         D     &= R^T(A^TA)R              \\
--     => Id     &= D^{-1}R^T(A^TA)R        \\
--     => Id     &= RD^{-1}R^T(A^TA)   \\
--     => A^{-1} &= RD^{-1}R^TA^T      \\
--   \end{align*}

type End a = T a -> T a

inverse 
  :: (Show a, Eq a, Eq b, Ord a, 
      FiniteSet a, FiniteSet b, Show (T a -> T a)) 
  => (T a -> T b) -> T b -> T a
inverse a
  = let at = transpose a
        diags = [ (x,y) | x <- elements , y <- elements , x < y ]
        ls = LS (at . a) id diags
        (d,r) = loop ls
    in  r . invDiagonal d . transpose r . at

invDiagonal :: (Eq a) => End a -> End a
invDiagonal l
  = let coef x = let T v = l (return x) in v (delta x)
        base x = scale (recip $ coef x) (return x)
    in  extend base

data LoopState a
  = LS !(End a)      -- transpose $ D := A^TA  $
       !(End a)      -- rotation  $ R := Id    $
       [(a,a)]      -- diagonals that need checking

isZero :: R -> Bool
isZero r = abs r < 1e-8

loop
  :: (Show a, Eq a, Ord a, FiniteSet a, Show (T a -> T a)) 
  => LoopState a -> (End a, End a)
loop (LS d r []) = (d,r)
loop (LS d r (diag@(x,y):diags))
  = let c = traceShowId $ let T dx = d (return x) in dx (delta y)
    in if isZero c
          then loop (LS d r diags) 
          else let rot = makeRotation d diag
                   d'  = traceShowId $ transpose rot . d . rot
                   diags' 
                    = diags 
                        ++ [ (x,y') | y' <- elements, y /= y', x < y']
                        ++ [ (x',y) | x' <- elements, x /= x', x' < y]
               in  loop (LS d' (r . rot) diags')

makeRotation 
    :: (Eq a, FiniteSet a)
    => (T a -> T a) -> (a, a) -> (T a -> T a)
makeRotation m (x, y) = 
    let mc a b = let T mv = m (return a) in mv (delta b)
        ct = ((mc x x) - (mc y y)) / (2*(mc x y))
    in  rot x y (angle ct)

angle :: R -> R
angle ct = 
    let sgn a = a / abs a
    in atan $ (sgn ct) / ((abs ct) + (sqrt (1 + ct*ct)))

rot
  :: (Eq a, FiniteSet a) 
  => a -> a -> R -> End a
rot x y t
  = extend r
  where 
    st a = scale (sin t) (return a)
    ct a = scale (cos t) (return a)
    r i | x == i    =        ct x  `plus` st y
        | y == i    = minus (st x) `plus` ct y
        | otherwise = return i


-- Want to make a cheaper rotation, without transcendental functions
makeRotation2
  :: (Eq a, FiniteSet a) 
  => R -> a -> a -> End a
makeRotation2 t x y
  | t < -1    =  extend (r (-t)       (-1))
  | t > 1     =  extend (r   t          1 )
  | otherwise =  extend (r   1   (recip t))
  where
    tx = return x
    ty = return y
    r a b i | x == i    = scale a    tx `plus` scale b ty
            | y == i    = scale (-b) tx `plus` scale a ty
            | otherwise = return i


