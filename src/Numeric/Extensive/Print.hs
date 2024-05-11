{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Numeric.Extensive.Print where

import Data.List (find)

import Text.PrettyPrint.Boxes
import Text.Printf

import Numeric.Extensive.Core


showInBasis :: (Show b, Eq b) => [b] -> T b -> String
showInBasis bs v =
        let coef (T v') = v' . delta
            pairs = [ (e, coef v e) | e <- bs ]
            showPair (b, n)
               | n == " + 1" = " + "  ++ show b
               | n == " - 1" = " - "  ++ show b
               | n == " - 0" = " 0 "
               | n == " + 0" = " 0 "
               | otherwise   = n      ++ show b
        in  case map (showPair . showN) . filter (\(_,n) -> n /= 0.0) $ pairs of
                  [] -> " 0"
                  ss -> concat ss

showN :: (a, R) -> (a, String)
showN (b, n') =
    let --n = (read $ printf "%0.5f" n' ) :: Double
        n = n'
        rn = round n :: Integer
        i = n == fromInteger rn
        sgn
          | n == 0     = ""
          | n > 0      = " + "
          | otherwise  = " - "
        sn = if i then show (abs rn) else showSurds (abs n)
    in (b, sgn ++ sn)

instance (Eq a, FiniteSet a, Show a) => Show (T a) where
    show = showInBasis elements

mkBox :: (FiniteSet a, FiniteSet b, Eq b, Eq a)
      => T (Hom a b) -> Box
mkBox m = box
      where
        es = map return elements
        box = hsep 2 left cls
        cls = [ vsep 0 right (map (text . snd . showN) (coefficients (apply m e'))) | e' <- es]

printMap :: (FiniteSet a, FiniteSet b, Eq b, Eq a)
         =>  (T a -> T b) -> IO ()
printMap  = putStrLn . render . mkBox . hom
instance (FiniteSet a, FiniteSet b, Eq b, Eq a) => Show (T a -> T b) where
    show = render. mkBox . hom


surds :: [(R, String)]
surds
  = [ ( sqrt 2,         "√2")
    , ( 0.5,            "1/2")
    , ( 0.25,           "1/4")
    , ( 2 * sqrt 2,     "2√2")
    , ( sqrt (1/2),     "1/√2")
    , ( sqrt 3,         "√3")
    , ( sqrt (3/2),     "√3/√2")
    , ( 3 * sqrt (3/2), "3√3/√2")
    , ( sqrt 3,         "√3")
    , ( sqrt (1/3),     "1/√3")
    , ( sqrt (2/3),     "√2/√3")
    , ( sqrt 5,         "√5")
    , ( sqrt (1/5),     "1/√5")
    , ( sqrt 6,         "√3√2")
    , ( sqrt 7,         "√7")
    ]

showSurds :: R -> String
showSurds r
 = let ms = find (\(s,_) -> (abs (abs r - abs s)) < epsilon) surds
   in  maybe (printf "%0.6f" r) snd ms

