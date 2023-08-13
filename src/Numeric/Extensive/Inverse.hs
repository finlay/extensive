{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Numeric.Extensive.Inverse where

--import Debug.Trace
import Numeric.Algebra
import Prelude hiding ((+), (-), (*), (^), negate, (<), (>), sum, fromInteger, recip, (/))

import Numeric.Extensive.Core
import Numeric.Extensive.Print()

--   \begin{align*}
--         D     &= R^T(A^TA)R              \\
--     => Id     &= D^{-1}R^T(A^TA)R        \\
--     => Id     &= RD^{-1}R^T(A^TA)   \\
--     => A^{-1} &= RD^{-1}R^TA^T      \\
--   \end{align*}

type End a = T a -> T a

-- force the evaluation of the endomorphism (something like memoization)
force :: (Eq a, FiniteSet a, Eq b, FiniteSet b)
      => (T a -> T b) -> T a -> T b
force = apply . hom

inverse
  :: (Eq a, Eq b, Order a, FiniteSet a, FiniteSet b, Show a)
  => (T a -> T b) -> T b -> T a
inverse x = let (invx, _,_) = inversePre x  in invx

inversePre
  :: (Eq a, Eq b, Order a, FiniteSet a, FiniteSet b, Show a)
  => (T a -> T b) -> (T b -> T a, T a -> T a, T a -> T a)
inversePre a
  = let at = transpose a
        diags = [ (x,y) | x <- elements , y <- elements , x < y ]
        ls = LS (force $ at . a) id diags
        (d,r) = loop ls
        inva = force $ r . invDiagonal d . transpose r . at
    in  (inva, r, d)

inversePost
  :: (Eq a, Eq b, Order b, FiniteSet a, FiniteSet b, Show b)
  => (T a -> T b) -> T b -> T a
inversePost a
  = let at = transpose a
        diags = [ (x,y) | x <- elements , y <- elements , x < y ]
        ls = LS (force $ a . at) id diags
        (d,r) = loop ls
    in  force $ at . r . invDiagonal d . transpose r
-- inversePost
--   :: (Eq a, Eq b, Ord b, FiniteSet a, FiniteSet b)
--   => (T a -> T b) -> (T b -> T a, End b, T a -> T b, End a)
-- inversePost a
--   = let at = transpose a
--         diags = [ (x,y) | x <- elements , y <- elements , x < y ]
--         ls = LS (force $ a . at) id diags
--         (d,r) = loop ls
--         sqrtd = sqrtDiagonal d
--         s = force $ invDiagonal2 sqrtd . transpose r . a
--         inva = force $ at . r . invDiagonal2 d . transpose r
--     in  (inva, r, sqrtd, s)


invDiagonal2
    :: (Eq a, FiniteSet a)
    => (T a -> T a) -> (T a -> T a)
invDiagonal2 l
  = let T homl = hom l
    in  apply (T (recip . homl))
invDiagonal :: (Eq a) => End a -> End a
invDiagonal l
  = let coef x = let T v = l (return x) in v (delta x)
        base x = if isZero (coef x)
                    then zero
                    else scale (recip $ coef x) (return x)
    in  extend base

sqrtDiagonal
    :: (Eq a, Eq b, FiniteSet a, FiniteSet b)
    => (T a -> T b) -> T a -> T b
sqrtDiagonal l
  = let T homl = hom l
    in  apply (T (sqrt . homl))

data LoopState a
  = LS (End a)      -- transpose $ D := A^TA  $
       (End a)      -- rotation  $ R := Id    $
       [(a,a)]      -- diagonals that need checking

isZero :: R -> Bool
isZero r = abs r < 1e-8

loop
  :: (Eq a, Order a, FiniteSet a, Show a)
  => LoopState a -> (End a, End a)
loop (LS d r []) = (d,r)
loop (LS d r (diag@(x,y):diags))
  = let c = let T dx = d (return x) in dx (delta y)
    in if isZero c
       then loop (LS d r diags)
       else let rot = makeRotation d diag
                d'  = transpose rot . d . rot
                newdiags = [ (x ,y') | y' <- elements
                                     , y /= y'
                                     , x  < y']
                        ++ [ (x',y ) | x' <- elements
                                     , x /= x'
                                     , x' < y ]
                diags' = diags ++ [ d | d <- newdiags, not (elem d diags)]
            in  loop (LS (force d') (force $ r . rot) diags')

makeRotation
    :: (Eq a, FiniteSet a)
    => (T a -> T a) -> (a, a) -> (T a -> T a)
makeRotation m (x, y) =
    let mc a b = let T mv = m (return a) in mv (delta b)
        ct = ((mc x x) - (mc y y)) / (2*(mc x y))
    in  rot x y (angle ct)

angle :: R -> R
angle ct =
    let sgn a = if a >= 0.0 then 1.0 else -1.0
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
  | t < (-1)  =  extend (r (-t)       (-1))
  | t > 1     =  extend (r   t          1 )
  | otherwise =  extend (r   1   (recip t))
  where
    tx = return x
    ty = return y
    r a b i | x == i    = scale a    tx `plus` scale b ty
            | y == i    = scale (-b) tx `plus` scale a ty
            | otherwise = return i


