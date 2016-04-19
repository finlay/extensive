{-# LANGUAGE FlexibleInstances #-}
module Numeric.Extensive.Print where

import Text.PrettyPrint.Boxes

import Numeric.Extensive.Core


showInBasis :: (Show b, Eq b) => [b] -> V b -> String
showInBasis bs v =
        let coef (V v') = v' . delta
            pairs = map (\e -> (e, coef v e)) bs
            showPair (b, n) 
               | n == " + 1" = " + "  ++ show b
               | n == " - 1" = " - "  ++ show b
               | otherwise   = n      ++ show b
            showN (b, n') = 
                let --n = (read $ intf "%0.5f" n' ) :: Double
                    n = n'
                    rn = round n :: Integer
                    i = n == fromInteger rn
                    sgn = if n > 0 then " + " else " - "
                    sn = if i then show (abs rn) else show (abs n)
                in (b, sgn ++ sn)
        in  case map (showPair . showN) . filter (\(_,n) -> n /= 0.0) $ pairs of 
                  [] -> " 0"
                  ss -> concat ss


instance (Eq a, FiniteSet a, Show a) => Show (V a) where
    show = let sh :: (Show a, Eq a) => [a] -> V a -> String
               sh = showInBasis 
           in  sh elements

mkBox :: (FiniteSet a, FiniteSet b, Eq b, Eq a) 
      => V (Hom a b) -> Box
mkBox m = box
      where
        es = map return elements
        box = hsep 2 left cls
        cls = [ vsep 0 right (map (ts . snd) (coefficients (apply m e'))) | e' <- es]
        ts = text . show

printMap :: (FiniteSet a, FiniteSet b, Eq b, Eq a) 
         =>  (V a -> V b) -> IO ()
printMap  = putStrLn . render . mkBox . hom
instance (FiniteSet a, FiniteSet b, Eq b, Eq a) => Show (V a -> V b) where
    show = render. mkBox . hom
