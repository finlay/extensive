import Data.List (nub, sortBy)

import Math.Combinat.Permutations


main :: IO ()
main = putStrLn "Hello"


-- This is a calculation of permutations of the module structures on H⊗H⊗H
-- There are six modules, and we want to figure out how to generate tau2 from
-- the base permutations.
--
-- We will take a set of generators, and do an exhaustive search.

tau2 :: Permutation
tau2 = transpositions 6 [(2,3),(4,5)]


t1, t2 :: Permutation
t1 = transpositions 6 [(1,3),(2,4)]
t2 = transpositions 6 [(3,5),(4,6)]


generate :: [ Permutation ] -> [ Permutation ]
generate ins =
  let
    generate' ins out =
      let potentials = [ multiplyPermutation i o | i <- ins, o <- out ]
      in
        case filter (\p -> not (elem p out)) potentials of
          []    -> sortBy compare $ nub out
          fpots -> generate' ins (out ++ fpots)
  in  generate' ins [ identityPermutation 6 ]


c1,c2,c3 :: Permutation
c1 = transpositions 6 [(1,2)]
c2 = transpositions 6 [(3,4)]
c3 = transpositions 6 [(5,6)]
