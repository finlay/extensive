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



