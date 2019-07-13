{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

import GHC.TypeLits
import Data.Proxy
import System.Random
import qualified Test.QuickCheck as QC
import Criterion.Main

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import qualified Prelude

import Numeric.Extensive

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
      normalise :: R -> R
      normalise a =
        let s = signum a
            v = abs a  
        in if v == 0
            then 0
            else s * (log v)
      sce :: T a -> IO (T a)
      sce b = fmap (\x ->  scale (normalise x) b) (QC.generate QC.arbitrary )

randomMatrix 
    :: (FiniteSet a, Eq a, FiniteSet b, Eq b) 
    => Double -> IO (T a -> T b)
randomMatrix p = apply <$> randomElement p

runTest :: Integer -> Double -> IO String
runTest i p = do
    case someNatVal i of
        Nothing -> return ""
        Just (SomeNat (_ :: Proxy n)) -> do
            a :: T (N n) -> T (N n) <- randomMatrix p
            return $ show  a
    
main :: IO()
main 
  = defaultMain 
    [ bgroup (show n)
      [ bench (show p) $ nfIO $ runTest n p
      | p <- [0.0,0.1 .. 1.0 ] ] 
    | n <- [ 1 .. 10 ] ]

test :: IO ()
test = do
  m <- randomMatrix 0.7 :: IO (T (N 6) -> T (N 6))
  print m  
  let invm = inverse m
  print invm  
  print (invm . m)
