import Prelude hiding ((+), (-), (*), (^), (/), negate, (>), (<), sum, fromInteger)

import qualified Test.QuickCheck as QC

import Numeric.Extensive
import Numeric.SU3

-- Experiment to create basis for su3, randomly

-- choose an arbitray element of SU3
choose_su3 :: IO (T SU3)
choose_su3 = QC.generate QC.arbitrary1

-- normalise to have killing norm of -3
normalise_su3 :: T SU3 -> T SU3
normalise_su3 i =
  let b = abs $ killing (*) i i
  in  scale (sqrt 3 / sqrt b) i

-- Find orthogonal component
orthogonal :: T SU3 -> T SU3 -> T SU3
orthogonal i j =
  let bij = killing (*) i j
      bii = killing (*) i i
  in  j - scale (bij/bii) i


generate_su3 :: IO [T SU3]
generate_su3 = do
  -- (1)
  i' <- choose_su3
  -- (2)
  let i = normalise_su3 i'
  -- (3)
  j' <- choose_su3
  -- (4)
  let j = normalise_su3 $ orthogonal i' j'
  -- (5)
  let k = normalise_su3 (i * j)

  return [i, j, k]


main :: IO (T SU3, T SU3, T SU3)
main = do
  su3_basis <- generate_su3
  mapM_ (\(i,x) -> putStrLn $ show i ++ " = " ++ show x) $ zip [(1::Int)..] su3_basis
  let [i,j,k] = take 3 su3_basis
  putStrLn $ "(i*j)*k + (j*k)*i + (k*i)*j = " ++ (show $ (i*j)*k + (j*k)*i + (k*i)*j)
  putStrLn $ "killing i j = 0  => " ++ (show $ killing (*) i j == zero)
  putStrLn $ "killing j k = 0  => " ++ (show $ killing (*) j k == zero)
  putStrLn $ "killing k i = 0  => " ++ (show $ killing (*) k i == zero)
  putStrLn $ "killing i i = -3.0 => " ++ (show $ killing (*) i i == -3.0)
  putStrLn $ "killing j j = -3.0 => " ++ (show $ killing (*) j j == -3.0)
  putStrLn $ "killing k k = -3.0 => " ++ (show $ killing (*) k k == -3.0)
  return (i,j,k)

