{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Existential where

import Data.Functor.Identity

class RunTo m n where
  runTo :: m a -> n a

data T m b = forall a . T (m a) (a -> b)

runT :: RunTo m n => Functor n => T m b -> n b
runT (T a f) = f <$> runTo a

both :: Applicative m => T m b -> T m b -> T m (b, b)
T a0 f0 `both` T a1 f1
  = T ((,) <$> a0 <*> a1) $ \(a0, a1) -> (f0 a0, f1 a1)

instance (Applicative m, Monoid b) => Monoid (T m b) where
  mempty = T undefined (const mempty)
  T a0 f0 `mappend` T a1 f1
    = T ((,) <$> a0 <*> a1)
    $ \(a0, a1) -> f0 a0 `mappend` f1 a1

t :: T Identity String
t = T (pure ()) show `mappend` T (pure 2) show

instance RunTo Identity IO where
  runTo (Identity v) = do
    putStrLn "Now I'm running"
    return v

main :: IO ()
main = runT t >>= putStrLn
