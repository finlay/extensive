{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Random
import Data.Maybe (catMaybes)

import qualified Test.QuickCheck as QC

import Numeric.Extensive

-- Make a random matrix
randomElement :: (FiniteSet a) => IO (V a)
randomElement = 
 do 
    let choose b = do r :: Double <- randomRIO (0, 1)
                      if r > 0.6 then return $ Just b
                                 else return Nothing
    els <- catMaybes `fmap` mapM choose elements
    let sce b = fmap (\x -> scale x (return b)) (QC.generate QC.arbitrary )
    cs <- mapM sce els
    return $ foldl1 plus cs

randomMatrix :: (FiniteSet a, Eq a, FiniteSet b, Eq b) => IO (V a -> V b)
randomMatrix = fmap apply randomElement

data H = E | I | J | K deriving (Eq, Ord)
instance FiniteSet H where elements = [ E, I, J, K ]

data C = C Int deriving (Eq, Ord)
instance FiniteSet C where elements = [ C i | i <- [1 .. 2] ]
instance Show C where show (C i) = "C_"++show i 

main :: IO ()
main = do 
    
    -- Generate random three by three linear transformation
    (a :: V C -> V C)  <- randomMatrix
    --(a :: V (Tensor H H) -> V (Tensor H H))  <- randomMatrix
    putStrLn "A = "
    printMap a
    
    let (ainv, _) = inverse' a

    putStrLn "A^{-1} = "
    printMap ainv
-- 
--     putStrLn "Check..."
--     print (offNorm $ ainv . a)
-- 
-- 
--     putStrLn "Done"
-- 



