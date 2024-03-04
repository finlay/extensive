{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans -Wno-unused-top-binds #-}

import Prelude hiding ((+), (-), (*), (^), (/), negate, (>), (<), sum, fromInteger)
import Data.List
import Numeric.Extensive
import Numeric.Quaternion

import qualified Text.PrettyPrint.Boxes as Box

main :: IO ()
main = putStrLn "Does nothing here"

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


makey :: T H -> T H -> T HHH
makey x y = scale (1/(2 * sqrt 3)) $ symm $ x `tensor` x `tensor` y
yei, yie, yej, yje, yek, yke :: T HHH
yij, yji, yik, yki, yjk, ykj :: T HHH
yei = makey e i ; yie = makey i e
yej = makey e j ; yje = makey j e
yek = makey e k ; yke = makey k e
yij = makey i j ; yji = makey j i
yik = makey i k ; yki = makey k i
yjk = makey j k ; ykj = makey k j

yijk, yeij, yeik, yejk, yeee, yiii, yjjj, ykkk :: T HHH
yijk = symm $ i `tensor` j `tensor` k
yeij = symm $ e `tensor` i `tensor` j
yeik = symm $ e `tensor` i `tensor` k
yejk = symm $ e `tensor` j `tensor` k
yeee = e `tensor` e `tensor` e
yiii = i `tensor` i `tensor` i
yjjj = j `tensor` j `tensor` j
ykkk = k `tensor` k `tensor` k

ys :: NamedElements HHH
ys = [ ("yei", yei) , ("yej", yej) , ("yek", yek)
     , ("yie", yie) , ("yje", yje) , ("yke", yke)
     , ("yij", yij) , ("yjk", yjk) , ("yki", yki)
     , ("yji", yji) , ("ykj", ykj) , ("yik", yik)
     , ("yijk", yijk) , ("yeij", yeij) , ("yejk", yejk)
     , ("yeik", yeik) , ("yeee", yeee) , ("yiii", yiii)
     , ("yjjj", yjjj) , ("ykkk", ykkk) ]

yes, yis :: [ T HHH ]
yes = [yei, yej, yek, yie, yje, yke]
yis = [yjk, yki, yij, ykj, yik, yji]

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





-- Useful function

showComm
  :: (Eq a, FiniteSet a, Show a)
  => NamedElements a -> (T a -> T a -> T a) -> [T a] -> [T a] -> IO ()
showComm nelms com left right  =
  let col  = Box.vsep 1 Box.right
      xs   = col ( Box.text "" : [Box.text (expandInto nelms x) | x <- left ])
      e1xs = [ col ( Box.text (expandInto nelms y) :
                [ Box.text (expandInto nelms (com x y)) | x <- left ])
             | y <- right ]
  in  putStrLn $ Box.render $ Box.hsep 2 Box.bottom ( xs: e1xs)



