{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Random
import qualified Test.QuickCheck as QC
import Numeric.Extensive

-- Make a random matrix
randomElement :: (FiniteSet a) => Double -> IO (T a)
randomElement p 
  = foldl1 plus <$> (mapM sce =<< els)
  where
      bernoulli :: (Num a, Ord a, Random a) => a -> IO Bool
      bernoulli p = (> p) <$> randomRIO (0, 1)
      choose :: Double -> a -> a -> IO a
      choose p a b =
          do r <- bernoulli p
             return $ if r then a else b
      els :: FiniteSet a => IO [T a]
      els = mapM (choose p zero . return) elements
      sce :: T a -> IO (T a)
      sce b = fmap (\x -> scale x b) (QC.generate QC.arbitrary )

randomMatrix 
    :: (FiniteSet a, Eq a, FiniteSet b, Eq b) 
    => Double -> IO (T a -> T b)
randomMatrix p = apply <$> randomElement p

data H = E | I | J | K deriving (Eq, Ord, Show)
instance FiniteSet H where elements = [ E, I, J, K ]

data C = C Int deriving (Eq, Ord)
instance FiniteSet C where elements = [ C i | i <- [1 .. 3] ]
instance Show C where show (C i) = "C_"++show i 

run :: Double -> IO ()
run p = do 
    
    -- Generate random three by three linear transformation
    --(a :: T C -> T C)  <- randomMatrix p
    (a :: T (Tensor C C) -> T (Tensor C C))  <- randomMatrix p
    --(a :: T (Tensor H H) -> T (Tensor H H))  <- randomMatrix p
    --(a :: T H -> T H)  <- randomMatrix p
    putStrLn "A = "
    printMap a
    
    let ainv = inverse a

    putStrLn "A^{-1} = "
    printMap ainv

    putStrLn "Check..."
    print (ainv . a)

    putStrLn "Done"
 

main :: IO()
main = run 0.5


