import Prelude hiding ((+), (-), (*), (^), (/), negate, (>), (<), sum, fromInteger)

import qualified Test.QuickCheck as QC

import Numeric.Extensive
import Numeric.Quaternion hiding (E, I, J, K, e, i, j, k)


--- Experiment to build an algorithm that creates an arbitrary embedding of {ijk} into so3
-- We have SO3 already here (XYZ)

-- choose an arbitray element of SO3
choose_so3 :: IO (T SO3)
choose_so3 = QC.generate QC.arbitrary1

-- normalise to have killing norm of -2
normalise_so3 :: T SO3 -> T SO3
normalise_so3 i =
  let b = abs $ killing (*) i i
  in  scale (sqrt 2 / sqrt b) i

-- Find orthogonal component
orthogonal :: T SO3 -> T SO3 -> T SO3
orthogonal i j =
  let bij = killing (*) i j
      bii = killing (*) i i
  in  j - scale (bij/bii) i


generate_ijk :: IO (T SO3, T SO3, T SO3)
generate_ijk = do
  -- (1)
  i' <- choose_so3
  -- (2)
  let i = normalise_so3 i'
  -- (3)
  j' <- choose_so3
  -- (4)
  let j = normalise_so3 $ orthogonal i' j'
  -- (5)
  let k = normalise_so3 (i * j)

  return (i, j, k)
