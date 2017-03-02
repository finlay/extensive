{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Numeric.Extensive.Inverse2 where

import Debug.Trace
import Numeric.Extensive.Core

--   \begin{align*}
--         D     &= (A^TA)R              \\
--     => Id     &= D^{-1}(A^TA)R        \\
--     => Id     &= R^{-1}D^{-1}(A^TA)   \\
--     => A^{-1} &= R^{-1}D^{-1}A^T      \\
--     => A^{-1} &= (DR)^{-1}A^T      \\
--   \end{align*}

type End a = T a -> T a

inverse2 
  :: (Eq a, Eq b, Ord a, FiniteSet a, FiniteSet b, Show (T a -> T a)) 
  => (T a -> T b) -> T b -> T a
inverse2 a
  = let at = transpose a
        diags = [ (x,y) | x <- elements , y <- elements , x < y ]
        ls = LS (at . a) id diags
        (d,r) = decompose ls
        recip' (T v) = T (recip . v)
        dinv = recip' . d
        rinv = transpose r
    in  rinv . dinv . at

data LoopState a
  = LS (End a)      -- transpose $ D := A^TA  $
       (End a)      -- rotation  $ R := Id    $
       [(a,a)]      -- diagonals that need checking

isZero :: R -> Bool
isZero r = abs r > 1e-8

decompose
  :: (Eq a, FiniteSet a, Show (T a -> T a)) 
  => LoopState a -> (End a, End a)
decompose (LS d r []) = (d,r)
decompose ls@(LS d r ((x,y):diags))
  = let T dx = d (return x) 
        c = traceShowId $ dx (delta y)
        ls' = if isZero c 
                then (LS d r diags) 
                else loop c ls
    in decompose ls'

loop
  :: (Eq a, FiniteSet a, Show (T a -> T a)) 
  => R -> LoopState a -> LoopState a
loop _ (LS _ _ []) = error "Can't get here"
loop c (LS d r ((x,y):diags))
  = let rot = traceShowId $ makeRotation (angle c) x y
        d' = d . rot
        r' = (transpose rot) . r
    in  LS d' r' (diags ++ [(x,y)])

angle :: R -> R
angle ct = 
    let sgn a = a / abs a
    in traceShowId $ atan $ (sgn ct) / ((abs ct) + (sqrt (1 + ct*ct)))

makeRotation
  :: (Eq a, FiniteSet a) 
  => R -> a -> a -> End a
makeRotation t x y
  = extend r
  where 
    st a = scale (sin t) (return a)
    ct a = scale (cos t) (return a)
    r i | x == i    =        ct x  `plus` st y
        | y == i    = minus (st x) `plus` ct y
        | otherwise = return i
