module Numeric.Extensive.Core where

import Control.Monad

import Test.QuickCheck (Arbitrary)
import qualified Test.QuickCheck as QC


type R = Double ; 
epsilon :: R
epsilon = 1e-6 -- fast and approximate 

newtype V a = V ((a -> R) -> R)

instance Functor V where 
    fmap f (V xs) = V $ \r -> xs (r . f)

instance Applicative V where
  pure a          = V $ \k -> k a
  V mf <*> V ma   = V $ \k -> mf $ \f -> ma $ k . f

instance Monad V where
    return x      = V $ \k -> k x
    (V x) >>= y   = V $ \k -> x $ \f -> let V yf = y f in yf k 

scale :: R  -> V a -> V a
scale r (V x) = V $ (r *) . x 

zero :: V a
zero = V $ const 0

minus :: V a -> V a
minus (V x) = V $ negate . x 

plus :: V a -> V a -> V a
plus (V x) (V y) = V $ (\ar -> x ar + y ar)

-- Extending a map into a the Vector space is easy peasy using the Monad instance
-- Automatically linear
extend :: (Monad m) => (a -> m b) -> m a -> m b
extend = flip (>>=)


-- Tensor products are just pairs
data Tensor a b = Tensor a b deriving (Eq, Ord)
tensor :: V a -> V b -> V (Tensor a b)
tensor tx ty =  (join . (fmap t') . t'') (Tensor tx ty)
    where
        t'' (Tensor x y) = fmap (Tensor x) y
        t'  (Tensor x y) = fmap (flip Tensor y) x

-- adj :: Hom(Va, Hom(Vb, Vc)) -> Hom(Va ⊗  Vb, Vc)
adj :: (V a -> (V b -> V c)) -> V (Tensor a b) -> V c
-- adj f (x ⊗ y) = f(x)(y) 
adj f = extend $ \(Tensor a b) -> f (return a) (return b)
-- iadj :: Hom(Va, Hom(Vb, Vc)) <- Hom(Va ⊗  Vb, Vc)
iadj :: (V (Tensor a b) -> V c) -> V a -> V b -> V c
-- iadj g x y = g (x ⊗ y)
iadj g va vb = g (va `tensor` vb)


-- Hom represent linear maps.
data Hom a b = Hom a b deriving (Eq, Ord)

-- This is an expensive operation (it calles elements)
hom :: (FiniteSet a, FiniteSet b, Eq b) 
    => (V a -> V b) -> V (Hom a b)
hom l = 
  let xs = elements
      coefs = coefficients . l . return
  in  foldl1 plus [scale c (return (Hom x' y')) | x' <- xs, (y',c) <- coefs x']

apply :: (Eq a) 
      => V (Hom a b) -> V a -> V b
apply = curry (unMaybe . fmap ex . uncurry tensor)
  where
    unMaybe = extend (maybe zero return)
    ex (Tensor (Hom x y) x') = {-# SCC "ex" #-} if x == x' then Just y else Nothing

em :: (Eq a) => Hom a b -> V a -> V b
em (Hom x y) (V vx) = 
  let em' vy x' = if x == x' then vy y else 0
  in  V $ vx . em'

-- memoised multiplication
mmul :: (Eq a, FiniteSet a, Eq c, FiniteSet c) 
     => (V b -> V c) -> (V a -> V b) -> (V a -> V c)
mmul a b =  {-# SCC "mmul" #-} apply . hom $ (a . b)

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

-- If we have a vector over a finite set, we can calculate the coefficients
coefficients :: (FiniteSet x, Eq x) => V x -> [(x, R)]
coefficients (V v) = map (\e -> (e, v (delta e))) elements

-- Equality instances are only ever approximate.
--instance (Eq a, FiniteSet a) => Eq (a -> R) where
--    x == y = all (\e -> x e == y e) elements

instance (Eq a, FiniteSet a) => Eq (V a) where
    x' == y' =  {-# SCC "EQUALS" #-} sum (map (squared . snd) ( coefficients (subtract' x' y'))) <= epsilon
              where 
                squared x'' = x'' * x''
                subtract' (V x'') (V y'') = V $ (\ar -> x'' ar - y'' ar)

instance (Eq a, FiniteSet a, Ord a) => Ord (V a) where
    compare x y = compare (coefficients x) (coefficients y)


-- Now we can talk about maps
delta :: Eq x => x -> x -> R
delta a b = {-# SCC "delta" #-} if a == b then 1 else 0

codual :: Eq a => V a -> (a -> R)
codual (V x)  = x . delta

-- return :: (a -> R) -> ((a -> R) -> R) -> R
-- fmap delta' :: ((a -> R) -> R) -> (((a -> R) -> R) -> R)
-- join   :: ((((a -> R) -> R) -> R) -> R) -> ((a -> R) -> R)

-- Expensive
dual :: (FiniteSet a, Eq a) => (a -> R) -> V a
dual x = V $ \y -> sum $ map (\e -> x e * y e) elements

dot :: Eq a => V a -> V a -> R
dot (V y) = y . codual

-- Transpose
transpose :: (FiniteSet a, FiniteSet b, Eq a, Eq b) 
          => (V a -> V b) -> (V b -> V a)
transpose lm = dual . (\b -> \a -> let V vb = lm $ return a in vb b) . codual


instance (Arbitrary a, Arbitrary b) => Arbitrary (Tensor a b) where
    arbitrary = Tensor <$> QC.arbitrary <*> QC.arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Hom a b) where
    arbitrary = Hom    <$> QC.arbitrary <*> QC.arbitrary

instance (Arbitrary a) => Arbitrary (V a)
  where
    arbitrary = 
      do
        bs    <- QC.listOf1 QC.arbitrary
        coefs <- QC.vector (length bs)
        return $ foldl1 plus $ map (\(n,b) -> scale n (return b)) $ zip coefs bs
