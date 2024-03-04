{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Prelude hiding ((+), (-), (*), (^), (/), negate, (>), (<), sum, fromInteger)
import Data.List
import Numeric.Extensive
import Numeric.Quaternion hiding (X, Y, Z)


import qualified Text.PrettyPrint.Boxes as Box

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

sk, sj, si, se :: T HHH
[sk, sj, si, se] = map (scale (-1)) $  take 4 $  nub $ sort $ image skew

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


data Y = Y H H H deriving (Ord, Eq)
instance Order Y where
    order a b = Just (compare a b)
instance Show Y where
    show (Y  a b c) = "Y"++ show a ++ show b ++ show c ++""

y0,y1,y2 :: [ Y ]
y0 = [ Y x x x | x <- elements ]
y1 = [ Y x x y | x <- elements, y <- elements, x /= y ]
y2 = [ Y x y z | x <- elements, y <- elements, z <- elements, x<y, y<z ]

instance FiniteSet Y where
    elements =  y0 ++ y1 ++ y2

ys :: [T Y]
ys = map return elements

injectY :: T Y -> T HHH
injectY = extend injectY'
  where
    injectY' (Y a b c) =
      let a' = return a
          b' = return b
          c' = return c
      in  symm $ a' `tensor` b' `tensor` c'

injectYInv :: T HHH -> T Y
injectYInv = force $ inverse injectY

instance Multiplicative (T Y) where
    (*) x' y' = injectYInv ((injectY x') * (injectY y'))


-- YA - six dimensional Lie sub-algebra
yeei, yjjj, yjjk, ykkj, ykkk :: T Y
yiii :: T Y
yeei = return $ Y E E I
yjjj = return $ Y J J J
yjjk = return $ Y J J K
ykkj = return $ Y K K J
ykkk = return $ Y K K K
yiii = return $ Y I I I

ya :: [ T Y ]
ya = [ yeei, yiii, yjjj + ykkk, yjjj - ykkk, yjjk + ykkj, yjjk - ykkj ]


-- YB - six dimensional Lie sub-algebra
yeej, yiik, ykki :: T Y
yeej = return $ Y E E J
yiik = return $ Y I I K
ykki = return $ Y K K I

yb :: [ T Y ]
yb = [ yeej, yjjj, ykkk + yiii, ykkk - yiii, yiik + ykki, yiik - ykki ]


-- YC - six dimensional Lie sub-algebra
yeek, yiij, yjji :: T Y
yeek = return $ Y E E K
yiij = return $ Y I I J
yjji = return $ Y J J I

yc :: [ T Y ]
yc = [ yeek, ykkk, yiii + yjjj, yiii - yjjj, yiij + yjji, yiij - yjji ]

-- YA, YB, and YC are all the same sub algebra. All have dimension six
-- What next ?

yeex :: [ T Y ]
yeex = [ yeei, yeej, yeek ]  -- Isomorphic to so3

yxxx :: [ T Y ]
yxxx = [ yiii, yjjj, ykkk ]  -- Isomorphic to so3

yxxy :: [ T Y ]
yxxy = [ yeei, yiij, yiik ]  -- Isomorphic to so3

yxxz :: [ T Y ]
yxxz = [ yeej, yjji, yjjk ]  -- Isomorphic to so3

yxxw :: [ T Y ]
yxxw = [ yeek, ykki, ykkj ]  -- Isomorphic to so3

yaab :: [ T Y ]
yaab = [ yiij, yjjk, ykki ]  -- Isomorphic to so3

ybba :: [ T Y ]
ybba = [ yiik, ykkj, yjji ]  -- Isomorphic to so3



yjje, ykke, yejk, yijk, yiie :: T Y
yjje = return $ Y J J E
ykke = return $ Y K K E
yejk = return $ Y E J K
yijk = return $ Y I J K
yiie = return $ Y I I E

-- These four are isomorphic *three* dimensional sub-algebras
-- What are these Lie algebras?

yabc :: [ T Y ]
yabc = [ yeei - ykki, yeei - yjji, yjji - ykki, yijk ]

ycab :: [ T Y ]
ycab = [ yeej - ykkj, yeej - yiij, ykkj - yiij, yijk ]

ybca :: [ T Y ]
ybca = [ yeek - yjjk, yeek - yiik, yjjk - yiik, yijk ]

yxxe :: [ T Y ]
yxxe = [ yjje - ykke, yiie - ykke, yiie - yjje, yijk ]


---------------------------
-- These three are isomorphic *five* dimensional sub-algebras

yeij, yeik :: T Y
yeij = return $ Y E I J
yeik = return $ Y E I K

yzzy :: [ T Y ]
yzzy = [ yiii + yeei, yiii - yeei, yeij + yeik, yeij - yeik, yjji + ykki]

yzzx :: [ T Y ]
yzzx = [ yjjj + yeej, yjjj - yeej, yeij + yejk, yeij - yejk, yiij + ykkj]

yzzw :: [ T Y ]
yzzw = [ ykkk + yeek, ykkk - yeek, yeik + yejk, yeik - yejk, yiik + yjjk]



-- These are isomorphic *six* dimensional
-- Look like they are Z2 graded over yabc

ywwx :: [ T Y ]
ywwx = yabc ++ [ yiie - yjje,  yjje - ykke, ykke - yiie, yejk ]

ywwy :: [ T Y ]
ywwy = ycab ++ [ yiie - ykke,  ykke - yjje, yjje - yiie, yeik ]

ywwz :: [ T Y ]
ywwz = ybca ++ [ yiie - ykke,  ykke - yjje, yjje - yiie, yeij ]



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







showComm :: (Show a) => (a -> a -> a) -> [a] -> [a] -> IO ()
showComm com left right  =
  let col  = Box.vsep 1 Box.right
      xs   = col ( Box.text "" : [Box.text (show x) | x <- left ])
      e1xs = [ col ( Box.text (show y) : [Box.text (show (com x y)) | x <- left ]) | y <- right ]
  in  putStrLn $ Box.render $ Box.hsep 2 Box.bottom ( xs: e1xs)






