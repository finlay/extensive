{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans -Wno-unused-top-binds #-}

import Prelude hiding ((+), (-), (*), (^), (/), negate, (>), (<), sum, fromInteger)
import Data.List
import Numeric.Extensive
import Numeric.Quaternion


-- import qualified Text.PrettyPrint.Boxes as Box

-- \H\otimes\H\otimes\H
type HHH = Tensor (Tensor H H) H
ehhh :: [T HHH]
ehhh = map return elements

instance Multiplicative (T (Tensor (Tensor H H) H)) where
    (*) x' y' = extend muHHH (x' `tensor` y')
            where
                muHHH  (Tensor (Tensor (Tensor xe ye) ze) (Tensor (Tensor xe' ye') ze'))
                     = ((return xe) * (return xe')) `tensor` ((return ye) * (return ye'))
                                                    `tensor` ((return ze) * (return ze'))

image :: (T HHH -> T HHH) -> [T HHH]
image p = [ p x | x <- ehhh ]

sk, sj, si, se :: T HHH
[sk, sj, si, se] = map (scale (-1)) $  take 4 $  nub $ sort $ image skew

--------------------------------------------------------------------------------
-- skew symmetric elements
skew :: T HHH -> T HHH
skew = extend skew'
  where
    skew' :: HHH -> T HHH
    skew' (Tensor (Tensor x' y') z') =
        let  x = return x'
             y = return y'
             z = return z'
        in   x `tensor` y `tensor` z
           - y `tensor` x `tensor` z
           + y `tensor` z `tensor` x
           - z `tensor` y `tensor` x
           + z `tensor` x `tensor` y
           - x `tensor` z `tensor` y

-- Is a projection
-- hom $ (skew . skew) - (scale 6 . skew) == 0

-- Dimension on Image of skew is 4.
showSkew :: IO ()
showSkew = do
  mapM_ putStrLn [ show x <> " -> " <> (show $ skew x) | x <- ehhh]


--------------------------------------------------------------------------------
-- symmetric elements
symm :: T HHH -> T HHH
symm = extend symm'
  where
    symm' :: HHH -> T HHH
    symm' (Tensor (Tensor x' y') z') =
        let  x = return x'
             y = return y'
             z = return z'
        in   x `tensor` y `tensor` z
           + y `tensor` x `tensor` z
           + y `tensor` z `tensor` x
           + z `tensor` y `tensor` x
           + z `tensor` x `tensor` y
           + x `tensor` z `tensor` y

-- hom $ (symm . symm) - (scale 6 . symm) == 0

-- Dimension on Image of symm is
--      12 + 4 + 4 =
--  =   2 * (4 choose 2) + (4 choose 1) + (4 choose 3) = 20.
showSymm :: IO ()
showSymm = do
  mapM_ putStrLn [ show x <> " -> " <> (show $ symm x) | x <- ehhh]


yij, yji, yik, yki, yjk, ykj :: T HHH
yij = scale (1/2) $ symm $ i `tensor` i `tensor` j
yji = scale (1/2) $ symm $ j `tensor` j `tensor` i
yik = scale (1/2) $ symm $ i `tensor` i `tensor` k
yki = scale (1/2) $ symm $ k `tensor` k `tensor` i
yjk = scale (1/2) $ symm $ j `tensor` j `tensor` k
ykj = scale (1/2) $ symm $ k `tensor` k `tensor` j


--------------------------------------------------------------------------------
-- 1/3 (2 - sig - sig^2)
p2 :: T HHH -> T HHH
p2 = extend p2'
  where
    p2' :: HHH -> T HHH
    p2' (Tensor (Tensor x' y') z') =
        let  x = return x'
             y = return y'
             z = return z'
        in  scale 2 ( x `tensor` y `tensor` z)
               - y `tensor` z `tensor` x
               - z `tensor` x `tensor` y

-- Is a projection
-- hom $ (p2 . p2) - (scale 3 . p2) == 0

-- Dimension on Image of symm is
--      12 + 4 + 4 =
--  =   2 * (4 choose 2) + (4 choose 1) + (4 choose 3) = 20.
showP2 :: IO ()
showP2 = do
  mapM_ putStrLn [ show x <> " -> " <> (show $ p2 x) | x <- ehhh]







