{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Box
  ( Box()
  , box
  , both
  , entrench
  , project
  , transfer
  ) where

import Control.Monad

data Box m b = forall a . Box (m a) (a -> b)

instance Functor (Box m) where
  f `fmap` Box m p = Box m (f . p)

instance Applicative m => Applicative (Box m) where
  pure x = Box (pure x) id
  Box m0 a0 <*> Box m1 a1
    = Box ((,) <$> m0 <*> m1)
    $ \(a0', a1') -> a0 a0' (a1 a1')

instance Monad m => Monad (Box m) where
  return = pure
  Box m0 a0 >>= f = Box m id
    where
      m = join $ fmap (project . f . a0) m0

instance (Applicative m, Monoid b) => Monoid (Box m b) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b

box :: m a -> (a -> b) -> Box m b
box = Box

entrench :: m a -> Box m a
entrench act = Box act id

project :: Functor m => Box m b -> m b
project (Box a p) = p <$> a

transfer :: (forall a . m a -> n a) -> Box m t -> Box n t
transfer f (Box m t) = Box (f m) t

both :: Applicative m => Box m b0 -> Box m b1 -> Box m (b0, b1)
both a b = (,) <$> a <*> b
