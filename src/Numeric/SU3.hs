{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Numeric.SU3 where

import Numeric.Algebra hiding (zero)
import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)

import Numeric.Extensive


--------------------------------------------------------------------------------
-- Directly building out su3 directly for generators

-- We start with a nine dimensional algebra, because it makes more sense (sort of)
-- Even though the algebra is really eight dimensional
--
-- based on https://visuallietheory.blogspot.com/2012/
-- with some tweaks (see below) -- to the right of lines

data SU3 = Tx | Ty | Tz | Ux | Uy | Uz | Vx | Vy | Vz deriving (Ord, Eq)
instance Order SU3 where
    order a b = Just (compare a b)
instance Show SU3 where
    show Tx = "tx"
    show Ty = "ty"
    show Tz = "tz"
    show Ux = "ux"
    show Uy = "uy"
    show Uz = "uz"
    show Vx = "vx"
    show Vy = "vy"
    show Vz = "vz"

instance FiniteSet SU3 where
    elements = [ Tx, Ty, Tz, Ux, Uy, Uz, Vx, Vy, Vz ]

su3:: [T SU3]
su3 = map return elements
tx, ty, tz, ux, uy, uz, vx, vy, vz :: T SU3
[tx, ty, tz, ux, uy, uz, vx, vy, vz] = su3

su3_bracket :: T (Tensor SU3 SU3) -> T SU3
su3_bracket = extend su3_bracket'
  where
    su3_bracket' :: Tensor SU3 SU3 -> T SU3

    su3_bracket' (Tx `Tensor` Ty) = tz
    su3_bracket' (Tx `Tensor` Tz) = minus ty
    su3_bracket' (Ty `Tensor` Tx) = minus tz
    su3_bracket' (Ty `Tensor` Tz) = tx
    su3_bracket' (Tz `Tensor` Tx) = ty
    su3_bracket' (Tz `Tensor` Ty) = minus tx

    su3_bracket' (Tx `Tensor` Ux) = scale (  0.5) vy
    su3_bracket' (Tx `Tensor` Uy) = scale (- 0.5) vx
    su3_bracket' (Tx `Tensor` Uz) = scale (  0.5) ty
    su3_bracket' (Ty `Tensor` Ux) = scale (- 0.5) vx
    su3_bracket' (Ty `Tensor` Uy) = scale (- 0.5) vy    --- : + -> -
    su3_bracket' (Ty `Tensor` Uz) = scale (- 0.5) tx
    su3_bracket' (Tz `Tensor` Ux) = scale (- 0.5) uy
    su3_bracket' (Tz `Tensor` Uy) = scale (  0.5) ux

    su3_bracket' (Tx `Tensor` Vx) = scale (  0.5) uy
    su3_bracket' (Tx `Tensor` Vy) = scale (- 0.5) ux
    su3_bracket' (Tx `Tensor` Vz) = scale (- 0.5) ty
    su3_bracket' (Ty `Tensor` Vx) = scale (  0.5) ux    --- : - -> +
    su3_bracket' (Ty `Tensor` Vy) = scale (  0.5) uy
    su3_bracket' (Ty `Tensor` Vz) = scale (  0.5) tx
    su3_bracket' (Tz `Tensor` Vx) = scale (  0.5) vy
    su3_bracket' (Tz `Tensor` Vy) = scale (- 0.5) vx

    su3_bracket' (Ux `Tensor` Tx) = scale (- 0.5) vy
    su3_bracket' (Ux `Tensor` Ty) = scale (  0.5) vx
    su3_bracket' (Ux `Tensor` Tz) = scale (  0.5) uy
    su3_bracket' (Uy `Tensor` Tx) = scale (  0.5) vx
    su3_bracket' (Uy `Tensor` Ty) = scale (  0.5) vy    --- : - -> +
    su3_bracket' (Uy `Tensor` Tz) = scale (- 0.5) ux
    su3_bracket' (Uz `Tensor` Tx) = scale (- 0.5) ty
    su3_bracket' (Uz `Tensor` Ty) = scale (  0.5) tx

    su3_bracket' (Ux `Tensor` Uy) = uz
    su3_bracket' (Ux `Tensor` Uz) = minus uy
    su3_bracket' (Uy `Tensor` Ux) = minus uz
    su3_bracket' (Uy `Tensor` Uz) = ux
    su3_bracket' (Uz `Tensor` Ux) = uy
    su3_bracket' (Uz `Tensor` Uy) = minus ux

    su3_bracket' (Ux `Tensor` Vx) = scale (- 0.5) ty
    su3_bracket' (Ux `Tensor` Vy) = scale (  0.5) tx
    su3_bracket' (Ux `Tensor` Vz) = scale (- 0.5) uy
    su3_bracket' (Uy `Tensor` Vx) = scale (- 0.5) tx
    su3_bracket' (Uy `Tensor` Vy) = scale (- 0.5) ty    --- : + -> -
    su3_bracket' (Uy `Tensor` Vz) = scale (  0.5) ux
    su3_bracket' (Uz `Tensor` Vx) = scale (  0.5) vy
    su3_bracket' (Uz `Tensor` Vy) = scale (- 0.5) vx

    su3_bracket' (Vx `Tensor` Tx) = scale (- 0.5) uy
    su3_bracket' (Vx `Tensor` Ty) = scale (- 0.5) ux   ---  : + -> -
    su3_bracket' (Vx `Tensor` Tz) = scale (- 0.5) vy
    su3_bracket' (Vy `Tensor` Tx) = scale (  0.5) ux
    su3_bracket' (Vy `Tensor` Ty) = scale (- 0.5) uy
    su3_bracket' (Vy `Tensor` Tz) = scale (  0.5) vx
    su3_bracket' (Vz `Tensor` Tx) = scale (  0.5) ty
    su3_bracket' (Vz `Tensor` Ty) = scale (- 0.5) tx

    su3_bracket' (Vx `Tensor` Ux) = scale (  0.5) ty
    su3_bracket' (Vx `Tensor` Uy) = scale (  0.5) tx
    su3_bracket' (Vx `Tensor` Uz) = scale (- 0.5) vy
    su3_bracket' (Vy `Tensor` Ux) = scale (- 0.5) tx
    su3_bracket' (Vy `Tensor` Uy) = scale (  0.5) ty    --- : - -> +
    su3_bracket' (Vy `Tensor` Uz) = scale (  0.5) vx
    su3_bracket' (Vz `Tensor` Ux) = scale (  0.5) uy
    su3_bracket' (Vz `Tensor` Uy) = scale (- 0.5) ux

    su3_bracket' (Vx `Tensor` Vy) = vz
    su3_bracket' (Vy `Tensor` Vx) = minus vz
    su3_bracket' (Vy `Tensor` Vz) = vx
    su3_bracket' (Vz `Tensor` Vy) = minus vx
    su3_bracket' (Vz `Tensor` Vx) = vy
    su3_bracket' (Vx `Tensor` Vz) = minus vy

    su3_bracket' _ = zero

instance Multiplicative (T SU3) where
    (*) x' y' = su3_bracket (x' `tensor` y')


showSU3 :: T SU3 -> IO ()
showSU3 x
 = mapM_ putStrLn $ [ show x ++ " : " ++ show y ++ " -> " ++ show (x*y)
                    | y <- su3 ]
