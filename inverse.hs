{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Random

import Numeric.Extensive

-- Make a random matrix
randomElement :: (FiniteSet a) => IO (V a)
randomElement = 
 do 
    let sce b = fmap (flip scale (return b)) (randomRIO (-1, 1) )
    cs <- mapM sce elements
    return $ foldl1 plus cs

randomMatrix :: (FiniteSet a, Eq a, FiniteSet b, Eq b) => IO (V a -> V b)
randomMatrix = fmap apply randomElement

data H = E | I | J | K deriving (Eq, Ord)
instance FiniteSet H where elements = [ E, I, J, K ]

data C = C Int deriving (Eq, Ord)
instance FiniteSet C where elements = [ C i | i <- [1 .. 15] ]

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



