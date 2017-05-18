{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Box
  ( Box()
  , box
  , unbox
  , Transfer(..)
  ) where

import Data.Functor.Identity
import Control.Monad

data Box m b = forall a . Box (m a) (a -> b)

box :: m a -> (a -> b) -> Box m b
box = Box

both :: Applicative m => Box m b0 -> Box m b1 -> Box m (b0, b1)
Box a0 f0 `both` Box a1 f1
  = Box ((,) <$> a0 <*> a1) $ \(a0, a1) -> (f0 a0, f1 a1)

instance (Applicative m, Monoid b) => Monoid (Box m b) where
  mempty = Box undefined (const mempty)
  Box a0 f0 `mappend` Box a1 f1
    = Box ((,) <$> a0 <*> a1)
    $ \(a0, a1) -> f0 a0 `mappend` f1 a1

class Transfer m n where
  transfer :: m a -> n a

unbox :: Transfer m n => Functor n => Box m b -> n b
unbox (Box a f) = f <$> transfer a
