{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans -Wno-unused-top-binds #-}

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import Numeric.Extensive
import Numeric.Quaternion

import qualified Text.PrettyPrint.Boxes as Box


br :: T (Tensor H H) -> T (Hom H H)
br = extend $ hom . br'
  where
    br' :: Tensor H H -> T H -> T H
    br' (Tensor x y) tz = let tx = return x
                              ty = return y
                          in  tx * tz * ty

invbr :: T (Hom H H) -> T (Tensor H H)
invbr = inverse br

-- map Hom H H to Tensor H H*, where we equate H* with H (conjugated)
toDual :: T (Hom H H) -> T (Tensor H H)
toDual = extend toDual'
  where
    toDual' :: Hom H H -> T (Tensor H H)
    toDual' (Hom E x) = return $ Tensor x E
    toDual' (Hom y x) = scale (-1) $ return $ Tensor x y

fromDual :: T (Tensor H H) -> T (Hom H H)
fromDual = extend fromDual'
  where
    fromDual' :: Tensor H H -> T (Hom H H)
    fromDual' (Tensor x E) = hom $ em $ Hom E x
    fromDual' (Tensor x y) = scale (-1) $ hom $ em $ Hom y x



showMap :: (Show z, Show w) => (H -> H -> z) -> (z -> w) -> IO ()
showMap combine transform = do
  let showLine (x, y) =
         let xy = combine x y
         in  show xy ++ "  -->  " ++ show (transform xy)
      hs = elements :: [H]
  mapM_ (putStrLn . showLine) [ (x,y) | x <- hs, y <- hs]


showBr :: IO ()
showBr = showMap (\x -> \y -> return (Tensor x y) :: T (Tensor H H)) br

showInvBr :: IO ()
showInvBr = showMap (\x -> \y -> scale 4 $ return (Hom x y) :: T (Hom H H)) invbr

showBrDual :: IO ()
showBrDual = showMap (\x -> \y -> return (Tensor x y) :: T (Tensor H H)) (toDual . br)

showInvBrDual :: IO ()
showInvBrDual = showMap (\x -> \y -> scale 4 $ return (Tensor x y) :: T (Tensor H H)) (invbr . fromDual)

-- Show the map Br . Dual in tau basis.
showBrTau :: IO ()
showBrTau = do
  let transform = injectTauInv . scale 0.5 . toDual . br . injectTau
      showLine xy = show xy ++ "  -->  " ++ show (transform xy)
  mapM_ (putStrLn . showLine) tau

showBrTauInv :: IO ()
showBrTauInv = do
  let transform = injectTauInv  . invbr . fromDual . injectTau
      showLine xy = show xy ++ "  -->  " ++ show (transform xy)
  mapM_ (putStrLn . showLine) tau


main :: IO ()
main = do
  putStrLn "Brauer map"
  showBr
  putStrLn "Inverse Brauer map"
  showInvBr

showMap' :: (Show z, Show w) => (z -> w) -> [z] -> IO ()
showMap' transform zs = do
  let showLine z = show z ++ "  -->  " ++ show (transform z)
  mapM_ (putStrLn . showLine) zs

showBrDualTau :: [T Tau] -> IO ()
showBrDualTau = showMap' (injectTauInv . toDual . br . injectTau)


-- Show all full algebra of H Tensor H
showHTH :: IO ()
showHTH = do
  let showLine (x,y) = show x ++ " × " ++ show y ++ " = " ++ show (x * y)
  mapM_ (putStrLn . showLine) [ (x, y) | x <- tau, y <- tau ]


showHTH2 :: IO ()
showHTH2 = do
  let showLine (x,y) = "[" ++ show x ++ ", " ++ show y ++ " ] = " ++ show (x * y - y * x)
  mapM_ (putStrLn . showLine) [ (x, y) | x <- tau, y <- tau ]


showcomm :: (Show a) => (a -> a -> a) -> [a] -> [a] -> IO ()
showcomm com left right  =
  let col  = Box.vsep 1 Box.right
      xs   = col ( Box.text "" : [Box.text (show x) | x <- left ])
      e1xs = [ col ( Box.text (show y) : [Box.text (show (com x y)) | x <- left ]) | y <- right ]
  in  putStrLn $ Box.render $ Box.hsep 2 Box.bottom ( xs: e1xs)


-- Want to calculate what the full differential looks like as a jacobian.
-- In particular, we want to present in terms of div, curl, grad, etc.



br2 :: T (Tensor (Tensor H H) H) -> T (Hom (Tensor H H) H)
br2 = extend $ hom . br2'
  where
    br2'' :: T H -> T H -> T H -> Tensor H H -> T H
    br2'' tx ty tz (Tensor u v)  = let tu = return u
                                       tv = return v
                                   in tx * tu * ty * tv * tz
    br2' :: Tensor (Tensor H H) H -> T (Tensor H H) -> T H
    br2' (Tensor (Tensor x y) z) = let tx = return x
                                       ty = return y
                                       tz = return z
                                   in  extend (br2'' tx ty tz)


invbr2 :: T (Hom (Tensor H H) H) -> T (Tensor (Tensor H H) H)
invbr2 = inverse br2

showMap2 :: (Show z, Show w) => (H -> H -> H -> z) -> (z -> w) -> IO ()
showMap2 combine transform = do
  let showLine (x, y, z) =
         let xyz = combine x y z
         in  show xyz ++ "  -->  " ++ show (transform xyz)
      hs = elements :: [H]
  mapM_ (putStrLn . showLine) [ (x,y,z) | x <- hs, y <- hs, z <- hs]

mk3 :: R -> H -> H -> H -> T (Tensor (Tensor H H) H)
mk3 r x y z = scale r $ return (Tensor (Tensor x y) z) :: T (Tensor (Tensor H H) H)

showBr2 :: IO ()
showBr2 = showMap2 (mk3 1.0) br2

showInvBr2 :: IO ()
showInvBr2 = showMap2 (\x -> \y -> \z -> scale 16 $ return (Hom (Tensor x y) z) :: T (Hom (Tensor H H) H)) invbr2


tau2 :: T (Hom (Tensor H H) H) -> T (Hom (Tensor H H) H)
tau2 = extend tau2'
  where
    tau2' :: Hom (Tensor H H) H -> T (Hom (Tensor H H) H)
    tau2' (Hom (Tensor x y) z) = return (Hom (Tensor y x) z)

taut :: T (Tensor (Tensor H H) H) -> T (Tensor (Tensor H H) H)
taut = invbr2 . tau2 . br2

showBrTau2 :: IO ()
showBrTau2 = showMap2 (mk3 4) taut

mul2 :: T (Tensor (Tensor H H) H) -> T H
mul2 = extend $ \(Tensor (Tensor x y) z) -> return x * return y * return z


sset :: [ T ( Tensor (Tensor H H) H) ]
sset =
  let hs = [e, i, j, k]
  in  [ a `tensor` b `tensor` c
      | a <- hs, b <- hs, c <- hs
      , a * b * c == e || a * b * c == scale (-1) e
      ]

showSSET :: IO ()
showSSET = do
  let showLine x = show x ++ " --> " ++ show (taut x)
  mapM_ (putStrLn . showLine) [scale 4 x | x <- sset]


-- Diagonalise taut
--
-- Remember that taut . taut = Id. It is idempotent.
-- The two eiganspaces of taut, which are defined by (id + taut) and (id - taut)
-- correspond to the anti-symmetric and symmetric parts of the first two factors
-- in H* ⊗ H* ⊗ H. The anti-symmetric part maps to Ω²(H), which has dimension 24.
--
-- The subset sset has 16 elements, or a 1/4 of the whole of H³. The implication
-- is that the image of (Id - taut) should have dimension 24/4 = 6.
-- Lets find those six dimensions.

i3, j3, k3 :: T (Tensor (Tensor H H) H)
i3 = i ⊗ i ⊗ e + i ⊗ e ⊗ i + e ⊗ i ⊗ i
j3 = j ⊗ j ⊗ e + j ⊗ e ⊗ j + e ⊗ j ⊗ j
k3 = k ⊗ k ⊗ e + k ⊗ e ⊗ k + e ⊗ k ⊗ k

ijk, kji :: T (Tensor (Tensor H H) H)
ijk = i ⊗ j ⊗ k + j ⊗ k ⊗ i + k ⊗ i ⊗ j
kji = i ⊗ k ⊗ j + j ⊗ i ⊗ k + k ⊗ j ⊗ i

e3 :: T (Tensor (Tensor H H) H)
e3 = e ⊗ e ⊗ e


-- Turns out that
-- i3 - j3, j3 - k3, and k3 - i3 are all in the kernel of (id - taut)
-- This accounts for a 2 dimensional space.
-- Also, ijk + kji is in the kernel, accounting for 1 more dimension. (three to go)
--
-- (id - taut) $ (scale 3 $ ijk - kji) + (scale 2 $ i3 + j3 + k3) == 0
-- + 2e ⊗ i ⊗ i + 2i ⊗ i ⊗ e + 2i ⊗ e ⊗ i
-- + 2e ⊗ j ⊗ j + 2j ⊗ j ⊗ e + 2j ⊗ e ⊗ j
-- + 2e ⊗ k ⊗ k + 2k ⊗ e ⊗ k + 2k ⊗ k ⊗ e
-- + 3i ⊗ j ⊗ k + 3j ⊗ k ⊗ i + 3k ⊗ i ⊗ j
-- - 3i ⊗ k ⊗ j - 3j ⊗ i ⊗ k - 3k ⊗ j ⊗ i
-- This accounts for one more dimension. two to go!


-- hmmm, where to look now...
-- (id - taut) $ (scale 3 e3) - (scale 1 $ i3 + j3 + k3) == 0
-- + 3e ⊗ e ⊗ e
-- - e ⊗ i ⊗ i - i ⊗ e ⊗ i - i ⊗ i ⊗ e
-- - e ⊗ j ⊗ j - j ⊗ e ⊗ j - j ⊗ j ⊗ e
-- - e ⊗ k ⊗ k - k ⊗ e ⊗ k - k ⊗ k ⊗ e
-- One more dimesion down, one to go.


--  scale ( 0.000000000) e ⊗ e ⊗ e +
--
--  scale (-0.152732081) e ⊗ i ⊗ i +
--  scale ( 0.045751482) i ⊗ e ⊗ i +
--  scale ( 0.106980599) i ⊗ i ⊗ e +
--
--  scale ( 0.305455256) e ⊗ j ⊗ j +
--  scale ( 0.002533813) j ⊗ e ⊗ j +
--  scale (-0.307989068) j ⊗ j ⊗ e +
--
--  scale ( 0.259651019) e ⊗ k ⊗ k +
--  scale (-0.048285295) k ⊗ e ⊗ k +
--  scale (-0.211365724) k ⊗ k ⊗ e +
--
--  scale ( 0.366631618) i ⊗ j ⊗ k +
--  scale ( 0.094089531) k ⊗ i ⊗ j +
--  scale (-0.460721150) j ⊗ k ⊗ i +
--
--  scale (-0.412435855) i ⊗ k ⊗ j +
--  scale ( 0.048338049) j ⊗ i ⊗ k +
--  scale ( 0.364097805) k ⊗ j ⊗ i +
--
ia, ib, ja, jb, ka, kb, ij, jk, kj, ik :: T (Tensor (Tensor H H) H)
ia = scale (-1) e ⊗ i ⊗ i + scale ( 0.5) i ⊗ e ⊗ i + scale ( 0.5) i ⊗ i ⊗ e
ib = scale (-1) i ⊗ i ⊗ e + scale ( 0.5) i ⊗ e ⊗ i + scale ( 0.5) e ⊗ i ⊗ i
ja = scale (-1) e ⊗ j ⊗ j + scale ( 0.5) j ⊗ e ⊗ j + scale ( 0.5) j ⊗ j ⊗ e
jb = scale (-1) j ⊗ j ⊗ e + scale ( 0.5) j ⊗ e ⊗ j + scale ( 0.5) e ⊗ j ⊗ j
ka = scale (-1) e ⊗ k ⊗ k + scale ( 0.5) k ⊗ e ⊗ k + scale ( 0.5) k ⊗ k ⊗ e
kb = scale (-1) k ⊗ k ⊗ e + scale ( 0.5) k ⊗ e ⊗ k + scale ( 0.5) e ⊗ k ⊗ k
ij = scale (-1) i ⊗ j ⊗ k + scale ( 0.5) k ⊗ i ⊗ j + scale ( 0.5) j ⊗ k ⊗ i
jk = scale (-1) j ⊗ k ⊗ i + scale ( 0.5) i ⊗ j ⊗ k + scale ( 0.5) k ⊗ i ⊗ j
kj = scale (-1) k ⊗ j ⊗ i + scale ( 0.5) i ⊗ k ⊗ j + scale ( 0.5) j ⊗ i ⊗ k
ik = scale (-1) i ⊗ k ⊗ j + scale ( 0.5) j ⊗ i ⊗ k + scale ( 0.5) k ⊗ j ⊗ i

-- taut (scale 2 ia)  =         jb        + kb         - jk         + kj
-- taut (scale 2 ib)  =         ja        + ka         - ij         + ik
-- taut (scale 2 ja)  =   ib              + kb         + (ij + jk)  + ik
-- taut (scale 2 jb)  =   ia              + ka         - jk         - (ik + kj)
-- taut (scale 2 ka)  =   ib  + jb                     - ij         - (ik + kj)
-- taut (scale 2 kb)  =   ia  + ja                     + (ij + jk)  + kj
-- taut (scale 2 ij)  = - ib  + (ja + jb) - ka                      - kj
-- taut (scale 2 jk)  = - ia  - jb        + (ka + kb)               - ik
-- taut (scale 2 kj)  =   ia  - (ja + jb) + kb         - ij
-- taut (scale 2 ik)  =   ib  + ja        - (ka + kb)  - jk


-- for each of five clocks, there are three settings. We want to test all of them

test :: T (Tensor (Tensor H H) H) -> IO ()
test a
 = let d = taut a + a
   in  putStrLn $ "(taut + id)" ++ show a  ++ " -> " ++ show d

dotests :: IO ()
dotests
 = let combinations =
          [ a + b + c + d + e
          | a <- [ia, ib, ia+ib, scale (-1) ia, scale (-1) ib, scale (-1) (ia + ib)]
          , b <- [ja, jb, ja+jb, scale (-1) ja, scale (-1) jb, scale (-1) (ja + jb)]
          , c <- [ka, kb, ka+kb, scale (-1) ka, scale (-1) kb, scale (-1) (ka + kb)]
          , d <- [ij, jk, ij+jk, scale (-1) ij, scale (-1) jk, scale (-1) (ij + jk)]
          , e <- [kj, ik, kj+ik, scale (-1) kj, scale (-1) ik, scale (-1) (kj + ik)]
          ]
   in  mapM_ test combinations


--  +     e ⊗ i ⊗ i
--  - 1/2 i ⊗ e ⊗ i
--  - 1/2 i ⊗ i ⊗ e
--  - 1/2 e ⊗ j ⊗ j
--  +     j ⊗ e ⊗ j
--  - 1/2 j ⊗ j ⊗ e
--  +     e ⊗ k ⊗ k
--  - 1/2 k ⊗ e ⊗ k
--  - 1/2 k ⊗ k ⊗ e
--  + 1/2 i ⊗ j ⊗ k
--  -     k ⊗ i ⊗ j
--  + 1/2 j ⊗ k ⊗ i
--  +     i ⊗ k ⊗ j
--  - 1/2 j ⊗ i ⊗ k
--  - 1/2 k ⊗ j ⊗ i
--
-- (taut + id) $ ia - (ja + jb) + ka + (ij + jk) + ik == 0
--
--
--  - 1/2 e ⊗ i ⊗ i
--  +     i ⊗ e ⊗ i
--  - 1/2 i ⊗ i ⊗ e
--  +     e ⊗ j ⊗ j
--  - 1/2 j ⊗ e ⊗ j
--  - 1/2 j ⊗ j ⊗ e
--  +     e ⊗ k ⊗ k
--  - 1/2 k ⊗ e ⊗ k
--  - 1/2 k ⊗ k ⊗ e
--  + 1/2 i ⊗ j ⊗ k
--  + 1/2 k ⊗ i ⊗ j
--  -     j ⊗ k ⊗ i
--  - 1/2 i ⊗ k ⊗ j
--  - 1/2 j ⊗ i ⊗ k
--  +     k ⊗ j ⊗ i
--
-- (taut + id) $ (ia + ib) - ja - ka + jk - kj == 0
--
--
--  + 1/2 e ⊗ i ⊗ i
--  + 1/2 i ⊗ e ⊗ i
--  -     i ⊗ i ⊗ e
--  + 1/2 e ⊗ j ⊗ j
--  -     j ⊗ e ⊗ j
--  + 1/2 j ⊗ j ⊗ e
--  + 1/2 e ⊗ k ⊗ k
--  + 1/2 k ⊗ e ⊗ k
--  -     k ⊗ k ⊗ e
--  - 1/2 i ⊗ j ⊗ k
--  - 1/2 k ⊗ i ⊗ j
--  +     j ⊗ k ⊗ i
--  + 1/2 i ⊗ k ⊗ j
--  -     j ⊗ i ⊗ k
--  + 1/2 k ⊗ j ⊗ i
--
-- (taut + id) $ ib - (ja + jb) + kb - jk - (ik + kj) == 0
--
--
--
--  + 1/2 e ⊗ i ⊗ i
--  -     i ⊗ e ⊗ i
--  + 1/2 i ⊗ i ⊗ e
--  + 1/2 e ⊗ j ⊗ j
--  + 1/2 j ⊗ e ⊗ j
--  -     j ⊗ j ⊗ e
--  + 1/2 e ⊗ k ⊗ k
--  + 1/2 k ⊗ e ⊗ k
--  -     k ⊗ k ⊗ e
--  +     i ⊗ j ⊗ k
--  - 1/2 k ⊗ i ⊗ j
--  - 1/2 j ⊗ k ⊗ i
--  -     i ⊗ k ⊗ j
--  + 1/2 j ⊗ i ⊗ k
--  + 1/2 k ⊗ j ⊗ i
--
--
--
--
--
--  + 1/2 e ⊗ i ⊗ i
--  -     i ⊗ e ⊗ i
--  + 1/2 i ⊗ i ⊗ e
--  -     e ⊗ j ⊗ j
--  + 1/2 j ⊗ e ⊗ j
--  + 1/2 j ⊗ j ⊗ e
--  -     e ⊗ k ⊗ k
--  + 1/2 k ⊗ e ⊗ k
--  + 1/2 k ⊗ k ⊗ e
--  - 1/2 i ⊗ j ⊗ k
--  - 1/2 k ⊗ i ⊗ j
--  +     j ⊗ k ⊗ i
--  + 1/2 i ⊗ k ⊗ j
--  + 1/2 j ⊗ i ⊗ k
--  -     k ⊗ j ⊗ i
--
--
--  - 1/2 e ⊗ i ⊗ i
--  - 1/2 i ⊗ e ⊗ i
--  +     i ⊗ i ⊗ e
--  - 1/2 e ⊗ j ⊗ j
--  - 1/2 j ⊗ e ⊗ j
--  +     j ⊗ j ⊗ e
--  - 1/2 e ⊗ k ⊗ k
--  +     k ⊗ e ⊗ k
--  - 1/2 k ⊗ k ⊗ e
--  + 1/2 i ⊗ j ⊗ k
--  -     k ⊗ i ⊗ j
--  + 1/2 j ⊗ k ⊗ i
--  - 1/2 i ⊗ k ⊗ j
--  - 1/2 j ⊗ i ⊗ k
--  +     k ⊗ j ⊗ i
--
--
--
--
--
--  +     e ⊗ i ⊗ i
--  +     e ⊗ j ⊗ j
--  - 1/2 e ⊗ k ⊗ k
--  - 1/2 i ⊗ e ⊗ i
--  - 1/2 i ⊗ i ⊗ e
--  -     i ⊗ j ⊗ k
--  - 1/2 i ⊗ k ⊗ j
--  - 1/2 j ⊗ e ⊗ j
--  +     j ⊗ i ⊗ k
--  - 1/2 j ⊗ j ⊗ e
--  + 1/2 j ⊗ k ⊗ i
--  +     k ⊗ e ⊗ k
--  + 1/2 k ⊗ i ⊗ j
--  - 1/2 k ⊗ j ⊗ i
--  - 1/2 k ⊗ k ⊗ e
--
--
