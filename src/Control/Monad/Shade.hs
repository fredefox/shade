{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-| A control structure used to combine heterogenous types with delayed effects. -}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Shade
  ( ShadeT()
  , Shade
  , shade
  , hide
  , shadow
  , transfer
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.RWS.Class
import Control.Monad.Error.Class
import Control.Monad.Zip

import Data.Functor.Identity

-- | A shade consists of a hidden value and an image of that value. The hidden
-- values are stored in a context and cannot be accessed directly.
data ShadeT m b = forall a . ShadeT (m a) (a -> b)

type Shade = ShadeT Identity

-- | 'fmap' applies a function to the result of the projected value inside the
-- values original context.
instance Functor (ShadeT m) where
  f `fmap` ShadeT m p = ShadeT m (f . p)

-- | 'pure' is the identity projection of the original value stored in a pure
-- context.
--
-- @a '<*>' b@ combines the contexts of the hidden values and applies the shadow
-- of @b@ value onto the shadow of @a@.
instance Applicative m => Applicative (ShadeT m) where
  pure x = ShadeT (pure x) id
  ShadeT m0 p0 <*> ShadeT m1 p1
    = ShadeT ((,) <$> m0 <*> m1)
    $ \(a0, a1) -> p0 a0 (p1 a1)

-- | @m '>>=' f@ applies @f@ to the projected value inside the original context
-- of @m@. The result is the a shade which becomes the source object in the
-- result. This resut is nested twice inside the same context, and these are
-- joined together.
instance Monad m => Monad (ShadeT m) where
  return = pure
  ShadeT m0 p0 >>= f = ShadeT (join m) id
    where
      m = shadow . f . p0 <$> m0

-- | 'mempty' is simply the shadow and source of the neutral element of the
-- stored value.
--
-- 'mappend' combines the contexts of two shadows and mappends their stored
-- values.
instance (Applicative m, Monoid b) => Monoid (ShadeT m b) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b

-- | The projection of the hidden value (the "shadow").
shadow :: Functor m => ShadeT m b -> m b
shadow (ShadeT a p) = p <$> a

class MonadShade m where
  -- | Insert a contextual value and its projection into a shade.
  shade :: c a -> (a -> b) -> m c b
  -- | Changed the context of a hidden value. The first argument must be
  -- universally quantified since no assumptions may be made as to what value is
  -- stored inside the shade.
  transfer :: MonadShade m => (forall a . c0 a -> c1 a) -> m c0 t -> m c1 t

-- | Hide a boxed value inside a shade with the identity as projection.
hide :: MonadShade m => c a -> m c a
hide a = shade a id

instance MonadShade ShadeT where
  shade = ShadeT
  transfer f (ShadeT m t) = ShadeT (f m) t

-- An unfinished proof that `ShadeT` is indeed a monad transformer.
--
-- prop i.
--
--   lift . return
--     = \x -> lift . ShadeT (pure x) id
--     = \x -> (\a -> shade a id) . (ShadeT (pure x) id)
--     = \x -> ShadeT (ShadeT (pure x) id) id
--     = \x -> ShadeT (pure x) id
--     = return
--
-- prop ii.
--
--   lift (m >>= f)
--     = ShadeT (m >>= f) id
--
--   lift m >>= (lift . f)
--     = ShadeT m id >>= lift . f
--     = ShadeT (join (shadow . lift . f . id <$> m)) id
--     = ShadeT (join (shadow . lift . f <$> m)) id
instance MonadTrans ShadeT where
  lift = hide

instance MonadState s m => MonadState s (ShadeT m) where
  state = lift . state

instance MonadReader s m => MonadReader s (ShadeT m) where
  reader = lift . reader
  local f = lift . local f . shadow

instance MonadWriter s m => MonadWriter s (ShadeT m) where
  writer = lift . writer
  listen = lift . listen . shadow
  pass = lift . pass . shadow

instance MonadIO m => MonadIO (ShadeT m) where
  liftIO = lift . liftIO

instance Alternative m => Alternative (ShadeT m) where
  empty = hide empty
  a <|> b = hide $ shadow a <|> shadow b

instance MonadError e m => MonadError e (ShadeT m) where
  throwError = lift . throwError
  catchError act f = lift (shadow act `catchError` \e -> shadow (f e))

instance Monad m => MonadZip (ShadeT m) where
  mzip = liftM2 (,)
