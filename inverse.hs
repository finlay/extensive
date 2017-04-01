{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Random
import qualified Test.QuickCheck as QC
import Criterion.Main

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import qualified Prelude

import Numeric.Extensive
import Numeric.Quaternion

-- Make a random matrix
randomElement :: (FiniteSet a) => Double -> IO (T a)
randomElement p 
  = foldl1 plus <$> (mapM sce =<< els)
  where
      bernoulli :: (Num a, Ord a, Random a) => a -> IO Bool
      bernoulli p = (Prelude.> p) <$> randomRIO (0, 1)
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


data C = C Int deriving (Eq, Ord)
instance FiniteSet C where elements = [ C i | i <- [1 .. 3] ]
instance Show C where show (C i) = "C_"++show i 

main :: IO()
main = do
    (a5 :: T H -> T H)  <- randomMatrix 0.5
    (a4 :: T H -> T H)  <- randomMatrix 0.4
    (a3 :: T H -> T H)  <- randomMatrix 0.3
    (a2 :: T H -> T H)  <- randomMatrix 0.2
    (a1 :: T H -> T H)  <- randomMatrix 0.1
    let test = show . inverse
    defaultMain 
        [ bgroup "4"
            [ bench "0.5" $ nf test a5
            , bench "0.4" $ nf test a4
            , bench "0.3" $ nf test a3
            , bench "0.2" $ nf test a2
            , bench "0.1" $ nf test a1 ] ] 

