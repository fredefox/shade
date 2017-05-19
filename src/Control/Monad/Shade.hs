{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Shade
  ( Shade()
  , shade
  , hide
  , shadow
  , transfer
  ) where

import Control.Monad

-- | A shade consists of a hidden value and an image of that value. The hidden
-- values are stored in a context and cannot be accessed directly.
data Shade m b = forall a . Shade (m a) (a -> b)

instance Functor (Shade m) where
  -- | `fmap` applies a function to the result of the projected value inside the
  -- values original context.
  f `fmap` Shade m p = Shade m (f . p)

instance Applicative m => Applicative (Shade m) where
  -- | `pure` is the identity projection of the original value stored in a pure
  -- context.
  pure x = Shade (pure x) id
  -- | `<*>` combines the contexts of the hidden values and applies the shadow
  -- of the latter value onto the shadow of the former.
  Shade m0 p0 <*> Shade m1 p1
    = Shade ((,) <$> m0 <*> m1)
    $ \(a0, a1) -> p0 a0 (p1 a1)

instance Monad m => Monad (Shade m) where
  return = pure
  -- | `m >>= f` applies `f` to the projected value inside the original context
  -- of `m`. The result is the a shade which becomes the source object in the
  -- result. This resut is nested twice inside the same context, and these are
  -- joined together.
  Shade m0 p0 >>= f = Shade (join m) id
    where
      m = shadow . f . p0 <$> m0

instance (Applicative m, Monoid b) => Monoid (Shade m b) where
  -- | The neutral element is simply the shadow and source of the neutral
  -- element of the stored value.
  mempty = pure mempty
  -- | Mappend combines the contexts of two shadows and mappends their stored
  -- values.
  mappend a b = mappend <$> a <*> b

shade :: m a -> (a -> b) -> Shade m b
shade = Shade

-- | Hide a boxed value inside a shade with the identity as projection.
hide :: m a -> Shade m a
hide a = Shade a id

-- | The projection of the hidden value (the "shadow").
shadow :: Functor m => Shade m b -> m b
shadow (Shade a p) = p <$> a

-- | Changed the context of a hidden value. The first argument must be
-- universally quantified since no assumptions may be made as to what value is
-- stored inside the shade.
transfer :: (forall a . m a -> n a) -> Shade m t -> Shade n t
transfer f (Shade m t) = Shade (f m) t
