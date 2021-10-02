{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans #-}

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import Numeric.Extensive
import Numeric.Quaternion


br :: T (Tensor H H) -> T (Hom H H)
br = extend $ hom . br'
  where
    br' :: Tensor H H -> T H -> T H
    br' (Tensor x y) tz = let tx = return x
                              ty = return y
                          in  tx * tz * ty

invbr :: T (Hom H H) -> T (Tensor H H)
invbr = inverse br


showBr :: IO ()
showBr = do
  let showLine (x, y) =
         let xy = return (Tensor x y) :: T (Tensor H H)
         in  show xy ++ "  -->  " ++ show (br xy)
      hs = elements :: [H]
  mapM_ (putStrLn . showLine) [ (x,y) | x <- hs, y <- hs]

showInvBr :: IO ()
showInvBr = do
  let showLine (x, y) =
         let xy = scale 4 $ return (Hom x y) :: T (Hom H H)
         in  show xy ++ "  -->  " ++ show (invbr xy)
      hs = elements :: [H]
  mapM_ (putStrLn . showLine) [ (x,y) | x <- hs, y <- hs]


main :: IO ()
main = do
  putStrLn "Brauer map"
  showBr
  putStrLn "Inverse Brauer map"
  showInvBr

