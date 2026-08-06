{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-orphans -Wno-unused-top-binds #-}

import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import Data.List as L

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

conjugate2 :: T (Tensor H H) -> T (Tensor H H)
conjugate2 = extend conjugate2
  where
    conjugate2 (Tensor x y) = tensor (conjugate $ return x) (conjugate $ return y)

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
  let transform = injectTauInv  . scale 2.0 .  invbr . fromDual . injectTau
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

br2Dual  :: T (Tensor (Tensor H H) H) -> T (Hom (Tensor H H) H)
br2Dual l = hom $ apply (br2 l) . conjugate2

br2DualTau  :: T (Tensor (Tensor H H) H) -> T (Hom Tau H)
br2DualTau l = hom $ apply (br2 l) . (conjugate2 . injectTau )

conjugate3 :: T (Tensor (Tensor H H) H) -> T (Tensor (Tensor H H) H)
conjugate3 = extend conjugate3'
  where
    conjugate3' (Tensor x y) = tensor (conjugate2 $ return x) (conjugate $ return y)

sigma3 :: T (Tensor (Tensor H H) H) -> T (Tensor (Tensor H H) H)
sigma3 = extend sigma3'
  where
    sigma3' (Tensor (Tensor x y) z) = tensor (tensor (return z) (return y)) (return x)


mu3 :: T (Tensor (Tensor H H) H) -> T H
mu3 = extend mu3'
  where
    mu3' (Tensor (Tensor x y) z) = (return x) * (return y) * (return z)


invbr2 :: T (Hom (Tensor H H) H) -> T (Tensor (Tensor H H) H)
invbr2 = inverse br2

showMap2 :: (Show z, Show w) => ((H, H, H) -> z) -> (z -> w) -> IO ()
showMap2 combine transform = do
  let showLine (x, y, z) =
         let xyz = combine (x, y, z)
         in  show xyz ++ "  -->  " ++ show (transform xyz)
      hs = elements :: [H]
  mapM_ (putStrLn . showLine) [ (x,y,z) | x <- hs, y <- hs, z <- hs]

mk3 :: R -> (H, H, H) -> T (Tensor (Tensor H H) H)
mk3 r (x, y, z) = scale r $ return (Tensor (Tensor x y) z) :: T (Tensor (Tensor H H) H)

showBr2 :: IO ()
showBr2 = showMap2 (mk3 1.0) br2



showInvBr2 :: IO ()
showInvBr2 = showMap2 (\(x, y, z) -> scale 16 $ return (Hom (Tensor x y) z) :: T (Hom (Tensor H H) H)) invbr2


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

--- lets look at the preimage.


ddds :: [T (Tensor (Tensor H H) H)]
ddds =  (L.nub . L.sort) $ map (id + (sigma3 . conjugate3)) [ return x `tensor` return y `tensor` return z
                                                            | x <- elements, y <- elements, z <- elements]

-- mapM_ (\d -> putStrLn $ show d ++ "  -->  " ++ show (br2DualTau d)) ddds

ddss :: [T (Tensor (Tensor H H) H)]
ddss =  (L.nub . L.sort) $ map (id - (sigma3 . conjugate3)) [ return x `tensor` return y `tensor` return z
                                                            | x <- elements, y <- elements, z <- elements]

--
-- There are 24 elements to describe the elements of

ije, iji, ijj, ijk, jke, jki, jkj, jkk, kie, kii, kij, kik,
     eie, eii, eij, eik, eje, eji, ejj, ejk, eke, eki, ekj, ekk :: T (Tensor (Tensor H H) H)

ije = (id - (sigma3 . conjugate3)) $               (e ⊗ k ⊗ e - i ⊗ k ⊗ i - j ⊗ k ⊗ j - k ⊗ k ⊗ k) - scale (2.0) (e ⊗ e ⊗ k - i ⊗ e ⊗ j)
iji = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (j ⊗ k ⊗ k + e ⊗ k ⊗ i - i ⊗ e ⊗ k - e ⊗ e ⊗ j)
ijj = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ e ⊗ i - j ⊗ e ⊗ k + e ⊗ k ⊗ j - i ⊗ k ⊗ k)
ijk = (id + (sigma3 . conjugate3)) $               (e ⊗ e ⊗ e + i ⊗ e ⊗ i + j ⊗ e ⊗ j - k ⊗ e ⊗ k) + scale (2.0) (e ⊗ k ⊗ k + i ⊗ k ⊗ j)

jke = (id - (sigma3 . conjugate3)) $               (e ⊗ i ⊗ e - i ⊗ i ⊗ i - j ⊗ i ⊗ j - k ⊗ i ⊗ k) + scale (2.0) (j ⊗ e ⊗ k - e ⊗ e ⊗ i)
jki = (id + (sigma3 . conjugate3)) $               (e ⊗ e ⊗ e - i ⊗ e ⊗ i + j ⊗ e ⊗ j + k ⊗ e ⊗ k) + scale (2.0) (j ⊗ i ⊗ k + e ⊗ i ⊗ i)
jkj = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ i ⊗ j - i ⊗ e ⊗ j - e ⊗ e ⊗ k - i ⊗ i ⊗ k)
jkk = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ e ⊗ j + i ⊗ i ⊗ j + e ⊗ i ⊗ k - i ⊗ e ⊗ k)

kie = (id - (sigma3 . conjugate3)) $               (e ⊗ j ⊗ e - i ⊗ j ⊗ i - j ⊗ j ⊗ j - k ⊗ j ⊗ k) - scale (2.0) (e ⊗ e ⊗ j + i ⊗ e ⊗ k)
kii = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ e ⊗ k + j ⊗ j ⊗ k + e ⊗ j ⊗ i - i ⊗ e ⊗ j)
kij = (id + (sigma3 . conjugate3)) $               (e ⊗ e ⊗ e + i ⊗ e ⊗ i - j ⊗ e ⊗ j + k ⊗ e ⊗ k) + scale (2.0) (e ⊗ j ⊗ j - i ⊗ j ⊗ k)
kik = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ j ⊗ j + e ⊗ j ⊗ k - j ⊗ e ⊗ k - e ⊗ e ⊗ i)

eie = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ j ⊗ j - e ⊗ j ⊗ k + i ⊗ k ⊗ k + e ⊗ k ⊗ j)
eii = (id - (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ k ⊗ j - e ⊗ k ⊗ k - e ⊗ j ⊗ j - i ⊗ j ⊗ k)
eij = (id - (sigma3 . conjugate3)) $               (j ⊗ k ⊗ j - k ⊗ k ⊗ k - e ⊗ k ⊗ e - i ⊗ k ⊗ i) - scale (2.0) (j ⊗ j ⊗ k - e ⊗ j ⊗ i)
eik = (id - (sigma3 . conjugate3)) $               (e ⊗ j ⊗ e + i ⊗ j ⊗ i + j ⊗ j ⊗ j - k ⊗ j ⊗ k) + scale (2.0) (j ⊗ k ⊗ k + e ⊗ k ⊗ i)

eje = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (j ⊗ k ⊗ k - e ⊗ k ⊗ i - i ⊗ i ⊗ j + e ⊗ i ⊗ k)
eji = (id - (sigma3 . conjugate3)) $               (e ⊗ k ⊗ e - i ⊗ k ⊗ i + j ⊗ k ⊗ j + k ⊗ k ⊗ k) + scale (2.0) (i ⊗ i ⊗ k + e ⊗ i ⊗ j)
ejj = (id - (sigma3 . conjugate3)) $ scale (2.0) $ (j ⊗ i ⊗ k - e ⊗ i ⊗ i - i ⊗ k ⊗ j - e ⊗ k ⊗ k)
ejk = (id - (sigma3 . conjugate3)) $               (k ⊗ i ⊗ k - e ⊗ i ⊗ e - i ⊗ i ⊗ i - j ⊗ i ⊗ j) - scale (2.0) (i ⊗ k ⊗ k - e ⊗ k ⊗ j)

eke = (id + (sigma3 . conjugate3)) $ scale (2.0) $ (e ⊗ j ⊗ i - e ⊗ i ⊗ j - i ⊗ i ⊗ k - j ⊗ j ⊗ k)
eki = (id - (sigma3 . conjugate3)) $               (i ⊗ j ⊗ i - e ⊗ j ⊗ e - j ⊗ j ⊗ j - k ⊗ j ⊗ k) - scale (2.0) (i ⊗ i ⊗ j - e ⊗ i ⊗ k)
ekj = (id - (sigma3 . conjugate3)) $               (e ⊗ i ⊗ e + i ⊗ i ⊗ i - j ⊗ i ⊗ j + k ⊗ i ⊗ k) + scale (2.0) (i ⊗ j ⊗ j + e ⊗ j ⊗ k)
ekk = (id - (sigma3 . conjugate3)) $ scale (2.0) $ (i ⊗ j ⊗ k - e ⊗ j ⊗ j - e ⊗ i ⊗ i - j ⊗ i ⊗ k)


sk1 :: [T (Tensor (Tensor H H) H)]
sk1 = map (scale (0.5)) [ije, iji, ijj, ijk, jke, jki, jkj, jkk, kie, kii, kij, kik]
sk2 :: [T (Tensor (Tensor H H) H)]
sk2 = map (scale (0.5)) [eie, eii, eij, eik, eje, eji, ejj, ejk, eke, eki, ekj, ekk]





