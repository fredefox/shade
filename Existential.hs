{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Existential where

import Data.Functor.Identity
import Control.Monad

data T m b = forall a . T (m a) (a -> b)

both :: Applicative m => T m b0 -> T m b1 -> T m (b0, b1)
T a0 f0 `both` T a1 f1
  = T ((,) <$> a0 <*> a1) $ \(a0, a1) -> (f0 a0, f1 a1)

instance (Applicative m, Monoid b) => Monoid (T m b) where
  mempty = T undefined (const mempty)
  T a0 f0 `mappend` T a1 f1
    = T ((,) <$> a0 <*> a1)
    $ \(a0, a1) -> f0 a0 `mappend` f1 a1

class Transfer m n where
  transfer :: m a -> n a

runT :: Transfer m n => Functor n => T m b -> n b
runT (T a f) = f <$> transfer a

instance Transfer Identity IO where
  transfer (Identity v) = putStrLn "Transferring" *> pure v

teaWithSideFx :: String -> T Identity (IO ())
teaWithSideFx s = T (pure s) putStrLn

main :: IO ()
main = do
  r <- runT hetero
  putStrLn r
  join . runT . mconcat . map (teaWithSideFx . show) $ [0..10]
  where
    f x = T (pure x) show
    hetero :: T Identity String
    hetero = mconcat [ f () , f 2 , f "hej" ]
