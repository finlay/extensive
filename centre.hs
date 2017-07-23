{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans #-}
-- How to calculate the center of a Lie Algebra.

-- Borrowing ideas from deGraaf

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
--import qualified Prelude
import Numeric.Extensive
import Numeric.Quaternion
import Text.PrettyPrint.Boxes

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




--------------------------------------------------------------------------------
-- Experiments with H tensor H tensor H

type HT = T (Tensor (Tensor H H) H)
instance Multiplicative (T (Tensor (Tensor H H) H)) where
  (*) x' y' = extend muHHH (x' `tensor` y')
    where 
      muHHH (Tensor (Tensor (Tensor xe ye) ze) (Tensor ( Tensor xe' ye') ze'))
        = ((return xe) * (return xe')) 
                `tensor` ((return ye) * (return ye')) 
                `tensor` ((return ze) * (return ze'))

ijk :: [ T H ]
ijk = [i,j,k]
    
eee :: [ HT ]
eee = [ e `tensor` e `tensor` e ]
so31 :: [ HT ]
so31 = [ x `tensor` e `tensor` e | x <- ijk]
so32 :: [ HT ]
so32 = [ e `tensor` x `tensor` e | x <- ijk]
so33 :: [ HT ]
so33 = [ e `tensor` e `tensor` x | x <- ijk]


g33 :: [ HT ]
g33 = [ x `tensor` y `tensor` z
      | x <- ijk, y <- ijk, z <- ijk ]  


tbl  :: [ HT ] -> [ HT ] ->  Box
tbl xs ys = 
  let col = vsep 1 right
      lftcol = col (text "" : [text (show x) | x <- xs ])
      prods = [ col (text (show y) 
                    : [ text (show (x `comm` y)) | x <- xs ] 
                    ) | y <- ys ]
  in  hsep 3 bottom ( lftcol : prods )


p1 :: HT -> HT
p1 = extend p1'
  where
    p1' :: Tensor (Tensor H H) H -> HT
    p1' ( x `Tensor` y `Tensor` z)
        = return ( x `Tensor` y `Tensor` z)
        + return ( y `Tensor` z `Tensor` x)
        + return ( z `Tensor` x `Tensor` y)
        + return ( x `Tensor` z `Tensor` y)
        + return ( y `Tensor` x `Tensor` z)
        + return ( z `Tensor` y `Tensor` x)

p2 :: HT -> HT
p2 = extend p2'
  where
    p2' :: Tensor (Tensor H H) H -> HT
    p2' ( x `Tensor` y `Tensor` z)
        = return ( x `Tensor` y `Tensor` z)
        + return ( y `Tensor` z `Tensor` x)
        + return ( z `Tensor` x `Tensor` y)
        - return ( x `Tensor` z `Tensor` y)
        - return ( y `Tensor` x `Tensor` z)
        - return ( z `Tensor` y `Tensor` x)

p3 :: HT -> HT
p3 = extend p3'
  where
    p3' :: Tensor (Tensor H H) H -> HT
    p3' ( x `Tensor` y `Tensor` z)
        = (scale 2 $ return ( x `Tensor` y `Tensor` z))
        - return ( y `Tensor` z `Tensor` x)
        - return ( z `Tensor` x `Tensor` y)

tau :: HT -> HT
tau = extend tau'
  where
    tau' :: Tensor (Tensor H H) H -> HT
    tau' ( x `Tensor` y `Tensor` z)
        = return ( y `Tensor` x `Tensor` z)


--------------------------------------------------------------------------------
-- Pull back from Hom (Tensor H H) H
--------------------------------------------------------------------------------
toend :: HT -> T (Hom (Tensor H H) H)
toend = extend toend1
  where
    toend1 :: Tensor (Tensor H H) H -> T (Hom (Tensor H H) H)
    toend1 (x `Tensor` y `Tensor` z) = hom $ extend (appl x y z)
    appl :: H -> H -> H -> Tensor H H -> T H
    appl x y z (a `Tensor` b) =
        return x * return a * return y * return b * return z

invToend :: T (Hom (Tensor H H) H) -> HT
invToend = inverse toend


tauHHH :: (T (Tensor H H) -> T H) ->  T (Tensor H H) -> T H
tauHHH = (. ttau)
  where
    ttau :: End (Tensor H H)
    ttau = extend (\(x `Tensor` y) -> return (y `Tensor` x))






