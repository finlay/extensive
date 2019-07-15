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

(⊗) :: T a -> T b -> T (Tensor a b)
(⊗) = tensor


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


g12 :: [ HT ]
g12 = [ x `tensor` y `tensor` e
      | x <- ijk, y <- ijk ]

g13 :: [ HT ]
g13 = [ x `tensor` e `tensor` y
      | x <- ijk, y <- ijk ]

g23 :: [ HT ]
g23 = [ e `tensor` x `tensor` y
      | x <- ijk, y <- ijk ]

tbl  :: [ HT ] -> [ HT ] ->  Box
tbl xs ys =
  let col = vsep 1 right
      lftcol = col (text "" : [text (show x) | x <- xs ])
      prods = [ col (text (show y)
                    : [ text (show (x `comm` y)) | x <- xs ]
                    ) | y <- ys ]
  in  hsep 3 bottom ( lftcol : prods )


-- [di,dj] = 2 dk
-- [dj,dk] = 2 di
-- [dk,di] = 2 dj

diag :: T (Tensor H H)
diag = e `tensor` e + i `tensor` i + j `tensor` j + k `tensor` k

di, dj, dk :: HT
di =  scale (1 Prelude./2) $ p1 $ diag `tensor` i
dj =  scale (1 Prelude./2) $ p1 $ diag `tensor` j
dk =  scale (1 Prelude./2) $ p1 $ diag `tensor` k

-- [fi,fj] = 2 fk
-- [fj,fk] = 2 fi
-- [fk,fi] = 2 fj

fi, fj, fk :: HT
fi =  scale 3 $ p1 $ e `tensor` e `tensor` i
fj =  scale 3 $ p1 $ e `tensor` e `tensor` j
fk =  scale 3 $ p1 $ e `tensor` e `tensor` k

-- [di,fj] = 2 dk
-- [dj,fk] = 2 di
-- [dk,fi] = 2 dj



p1 :: HT -> HT
p1 = extend (scale (1 Prelude./ 6) . p1')
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
p2 = extend (scale (1 Prelude./ 6) . p2')
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
p3 = extend (scale (1 Prelude./ 3) . p3')
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
invToend = scale (1 Prelude./16) . transpose toend


tauHHH :: (T (Tensor H H) -> T H) ->  T (Tensor H H) -> T H
tauHHH = (. ttau)
  where
    ttau :: End (Tensor H H)
    ttau = extend (\(x `Tensor` y) -> return (y `Tensor` x))

l :: HT -> HT
l = invToend . hom . tauHHH .  apply . toend

allHT :: [ HT ]
allHT = basis





