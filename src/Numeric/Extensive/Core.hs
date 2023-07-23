{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Numeric.Extensive.Core where

import Control.Monad
import GHC.TypeLits
import Data.Proxy

import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC

import Text.Printf
import Numeric.Natural
import Numeric.Algebra
import Prelude hiding ((+), (-), (*), (^), negate, (>), (<), sum, fromInteger)
import qualified Prelude

newtype R = R Double
    deriving (Num, Ord, Fractional,
              Arbitrary, RealFrac, PrintfArg, Real, Floating)
epsilon :: R
epsilon = R 1e-6 -- fast and approximate

instance Eq R where
    x == y = abs (x - y) < epsilon

instance Show R where
    show (R n) = show n

instance Order R where
    order a b = Just (compare a b)

instance Additive R where
  (+) = (Prelude.+)
  --sinnum1p n r = (1 Prelude.+ toNatural n) * r

instance Abelian R

instance Division R where
  recip = Prelude.recip
  (/) = (Prelude./)
  (\\) = undefined -- not sure what this is supposed to be
  (^) = (Prelude.^^)

pow1pIntegral :: (Division r, Integral n) => r -> n -> r
pow1pIntegral r n = r ^ (1 Prelude.+ n)
instance Multiplicative R where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Semiring R

instance LeftModule  Natural R where (.*)   = (*) . fromIntegral
instance LeftModule  Integer R where (.*)   = (*) . fromIntegral
instance RightModule Natural R where m *. n = m * fromIntegral n
instance RightModule Integer R where m *. n = m * fromIntegral n

instance Monoidal R where
  zero = 0
  sinnum n r = fromIntegral n * r

instance Group R where
  (-) = (Prelude.-)
  negate = Prelude.negate
  subtract = Prelude.subtract
  times n r = fromIntegral n * r

instance Unital R where one = 1

instance Rig R where fromNatural = Prelude.fromIntegral
instance Ring R  where fromInteger = Prelude.fromInteger

newtype T a = T ((a -> R) -> R)

instance Functor T where
    fmap f (T xs) = T $ \r -> xs (r . f)

instance Applicative T where
  pure a          = T $ \k -> k a
  T mf <*> T ma   = T $ \k -> mf $ \f -> ma $ k . f

instance Monad T where
    return x      = T $ \k -> k x
    (T x) >>= y   = T $ \k -> x $ \f -> let T yf = y f in yf k

scale :: R  -> T a -> T a
scale r (T x) = T $ (r *) . x

instance Monoidal (T a) where
    zero = T $ \_ -> 0

minus :: T a -> T a
minus (T x) = T $ negate . x

plus :: T a -> T a -> T a
plus (T x) (T y) = T $ (\ar -> x ar + y ar)

instance LeftModule  Natural (T a) where n .* m = scale (fromIntegral n) m
instance LeftModule  Integer (T a) where n .* m = scale (fromIntegral n) m
instance RightModule Natural (T a) where m *. n = scale (fromIntegral n) m
instance RightModule Integer (T a) where m *. n = scale (fromIntegral n) m

instance Group (T a) where
  x - y = plus x (minus y)
  negate = minus
  subtract x y = plus (minus x) y
  times n r = scale (fromIntegral n) r


instance Additive (T v) where
    (+)   = plus

-- Extending a map into a the Vector space is easy peasy using the Monad instance
-- Automatically linear
extend :: (Monad m) => (a -> m b) -> m a -> m b
extend = flip (>>=)


-- Tensor products are just pairs
data Tensor a b = Tensor a b deriving (Eq, Ord)
tensor :: T a -> T b -> T (Tensor a b)
tensor tx ty =  (join . (fmap t') . t'') (Tensor tx ty)
    where
        t'' (Tensor x y) = fmap (Tensor x) y
        t'  (Tensor x y) = fmap (flip Tensor y) x

-- adj :: Hom(Va, Hom(Vb, Vc)) -> Hom(Va ⊗  Vb, Vc)
adj :: (T a -> (T b -> T c)) -> T (Tensor a b) -> T c
-- adj f (x ⊗ y) = f(x)(y)
adj f = extend $ \(Tensor a b) -> f (return a) (return b)
-- iadj :: Hom(Va, Hom(Vb, Vc)) <- Hom(Va ⊗  Vb, Vc)
iadj :: (T (Tensor a b) -> T c) -> T a -> T b -> T c
-- iadj g x y = g (x ⊗ y)
iadj g va vb = g (va `tensor` vb)


-- Hom represent linear maps.
data Hom a b = Hom a b deriving (Eq, Ord)

-- This is an expensive operation (it calles elements)
hom :: (FiniteSet a, FiniteSet b, Eq b)
    => (T a -> T b) -> T (Hom a b)
hom l =
  let xs = elements
      coefs = coefficients . l . return
  in  foldl1 plus [scale c (return (Hom x' y')) | x' <- xs, (y',c) <- coefs x']

apply :: (Eq a)
      => T (Hom a b) -> T a -> T b
apply = curry (unMaybe . fmap ex . uncurry tensor)
  where
    unMaybe = extend (maybe zero return)
    ex (Tensor (Hom x y) x') = {-# SCC "ex" #-} if x == x' then Just y else Nothing

em :: (Eq a) => Hom a b -> T a -> T b
em (Hom x y) (T vx) =
  let em' vy x' = if x == x' then vy y else 0
  in  T $ vx . em'

-- memoised multiplication
mmul :: (Eq a, FiniteSet a, Eq c, FiniteSet c)
     => (T b -> T c) -> (T a -> T b) -> (T a -> T c)
mmul a b =  {-# SCC "mmul" #-} apply . hom $ (a . b)

basis :: (FiniteSet a, Monad m) => [ m a ]
basis = map return elements

-- Finite sets can be listed, which is elements
class FiniteSet x where elements :: [ x ]

instance (FiniteSet x, FiniteSet y) => FiniteSet (Tensor x y) where
    elements = [ Tensor a b | a <- elements, b <- elements ]

instance (FiniteSet x, FiniteSet y) => FiniteSet (Hom x y) where
    elements = [ Hom a b | a <- elements, b <- elements ]

instance (Show x, Show y) => Show (Tensor x y) where
    show (Tensor x y) = show x ++ " \x2297 " ++ show y

instance (Show x, Show y) => Show (Hom x y) where
    show (Hom x y) = show x ++ " \x21A6 " ++ show y

-- Standard data type, parametrised by data kind int
newtype N (n::Nat) = N Integer
    deriving (Eq, Ord)

instance KnownNat n => FiniteSet (N n) where
    elements = let dim = natVal (Proxy :: Proxy n)
               in  [ N i | i <- [ 1 .. dim ] ]
instance Show (N n) where
    show (N i) = "n_"++show i

-- If we have a vector over a finite set, we can calculate the coefficients
coefficients :: (FiniteSet x, Eq x) => T x -> [(x, R)]
coefficients (T v) = map (\e -> (e, v (delta e))) elements

-- Equality instances are only ever approximate.
--instance (Eq a, FiniteSet a) => Eq (a -> R) where
--    x == y = all (\e -> x e == y e) elements

instance (Eq a, FiniteSet a) => Eq (T a) where
    x' == y' =  {-# SCC "EQUALS" #-} sum (map (squared . snd) ( coefficients (subtract' x' y'))) <= epsilon
              where
                squared x'' = x'' * x''
                subtract' (T x'') (T y'') = T $ (\ar -> x'' ar - y'' ar)

instance (Eq a, FiniteSet a, Ord a) => Ord (T a) where
    compare x y = compare (coefficients x) (coefficients y)


-- Now we can talk about maps
delta :: Eq x => x -> x -> R
delta a b = {-# SCC "delta" #-} if a == b then 1 else 0

codual :: Eq a => T a -> (a -> R)
codual (T x)  = x . delta

-- return :: (a -> R) -> ((a -> R) -> R) -> R
-- fmap delta' :: ((a -> R) -> R) -> (((a -> R) -> R) -> R)
-- join   :: ((((a -> R) -> R) -> R) -> R) -> ((a -> R) -> R)

-- Expensive
dual :: (FiniteSet a, Eq a) => (a -> R) -> T a
dual x = sum $ map (\e -> scale (x e) (return e)) elements

dot :: Eq a => T a -> T a -> R
dot (T y) = y . codual

-- Transpose
transpose :: (FiniteSet a, FiniteSet b, Eq a, Eq b)
          => (T a -> T b) -> (T b -> T a)
transpose lm = dual . (\b -> \a -> let T vb = lm $ return a in vb b) . codual


instance (Arbitrary a, Arbitrary b) => Arbitrary (Tensor a b) where
    arbitrary = Tensor <$> QC.arbitrary <*> QC.arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Hom a b) where
    arbitrary = Hom    <$> QC.arbitrary <*> QC.arbitrary

instance (Arbitrary a) => Arbitrary (T a)
  where
    arbitrary =
      do
        bs    <- QC.listOf1 QC.arbitrary
        coefs <- QC.vector (length bs)
        return $ foldl1 plus $ map (\(n,b) -> scale n (return b)) $ zip coefs bs
