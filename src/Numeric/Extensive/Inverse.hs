{-# LANGUAGE BangPatterns #-}
module Numeric.Extensive.Inverse where

import Control.Monad.State

import Numeric.Extensive.Core

-- --------------------
-- Inverses
-- uses a Jakobi approach

-- Create a rotation on an off diagonal for a endomorphism
rot :: Eq a => a -> a -> R -> T a -> T a
rot x' y' t = extend $ r'
  where 
    st a = scale (sin t) (return a)
    ct a = scale (cos t) (return a)
    r' i' | x' == i'  =        ct x'  `plus` st y'
          | y' == i'  = minus (st x') `plus` ct y'
          | otherwise = return i'


-- Given an off diagonal element, cacluate the angle
angle :: R -> R
angle ct = 
    let sgn a = a / abs a
    in atan $ (sgn ct) / ((abs ct) + (sqrt (1 + ct*ct)))

makeRotation :: Eq a => (T a -> T a) -> a -> a -> (T a -> T a)
makeRotation m x' y' = 
    let mc a b = let T mv = m (return a) in mv (delta b)
        ct = ((mc x' x') - (mc y' y')) / (2*(mc x' y'))
    in  rot x' y' (angle ct)

-- Diagonal element
data Diag a = Diag !a !a deriving (Eq, Show)
offdiag :: (FiniteSet a, Ord a) => [ Diag a ]
offdiag = [ Diag x' y' | x' <- elements, y' <- elements, x' < y']

diagStep :: (FiniteSet a, Eq a) 
         => Diag a -> (T a -> T a) -> (T a -> T a, T a -> T a)
diagStep (Diag r s) m = 
        let !t' = makeRotation m r s
            !m' = (transpose t') `mmul` m `mmul` t'
        in (m', t')

data DiagState a = DiagState { diags       :: [Diag a]
                             , transforms  :: [T a -> T a]
                             , result      :: T a -> T a 
                             , count       :: R
                             , diagsTotal  :: Int
                             , diagsLeft   :: Int
                             }
nextDiagStep :: (FiniteSet a, Eq a) => State (DiagState a) ()
nextDiagStep =
    let mc ma (Diag a b) = let T mav = ma (return a) in mav (delta b)
    in do
        s <- get
        let d:ds = diags s
        let ma   = result s
        if (abs $ mc ma d) > 1e-8 
            then do
                let (m, t) = diagStep d ma
                ts <- gets transforms
                put $ s { diags      = ds 
                        , transforms = t:ts
                        , result     = m
                        , count      = count s + 1
                        , diagsLeft  = diagsTotal s
                        }
            else do 
                let diags_left = diagsLeft s - 1
                if diags_left Prelude.> 0 
                    then do
                        put $ s { diags     = ds 
                                , diagsLeft = diags_left - 1
                                }
                        nextDiagStep
                    else 
                        return ()

-- Diagonalise a symmetric matrix
diagonaliseSym :: (FiniteSet a, Eq a, Ord a) 
               => R -> (T a -> T a) -> ((T a -> T a, [T a -> T a]), DiagState a)
diagonaliseSym maxcount ma = 
    let ds = offdiag
        dds = foldr1 (++) (repeat ds)
        initialState = DiagState { diags      = dds
                                 , transforms = [id]
                                 , result     = ma
                                 , count      = 0
                                 , diagsTotal = length ds
                                 , diagsLeft  = length ds
                                 }
        diagonalise = do
            m <- gets result
            c <- gets count
            if c < maxcount && offNorm m > 1/10^(8::Int) 
                then nextDiagStep >> diagonalise
                else do 
                    m'  <- gets result
                    ts <- gets transforms
                    return (m', ts)
    in  runState diagonalise initialState
       

offNorm :: (FiniteSet a, Eq a, Ord a) => (T a -> T a) -> R
offNorm m = 
  let mc (Diag a b) = let T ma = m (return a) in ma (delta b)
  in  sum $ map ((**2) . mc) offdiag


inverse' :: (FiniteSet a, Eq a, Ord a, FiniteSet b, Eq b, Ord b)
         => (T a -> T b) -> (T b -> T a, DiagState a)
inverse' l = 
    let ltl   = transpose l `mmul` l
        (steps, ds) = diagonaliseSym 1000 ltl
        lt    = foldl1 mmul (reverse $ snd steps)
        d     = fst steps
        T hd  = hom d 
        de s' = scale (1/(sqrt (hd (delta (Hom s' s'))))) (return (Hom s' s'))
        dinv  = apply $ (foldl1 plus [ de s | s <- elements ])
        rt    = l `mmul` lt `mmul` dinv
        linv  = lt `mmul` dinv `mmul` (transpose rt)
    in (linv, ds)
inverse :: (FiniteSet a, Eq a, Ord a, FiniteSet b, Eq b, Ord b)
        => (T a -> T b) -> T b -> T a
inverse = fst . inverse'


