> {-# LANGUAGE MultiParamTypeClasses #-}
> module Main ( main ) where
>
> import Data.Functor.Identity
> import Control.Monad
>
> import Control.Monad.Shade

A shade can be used to save heterogeneous types in containers with the same
type e.g. using a type-class as a common denominator:

> showshade :: Applicative m => Show a => a -> Shade m String
> showshade a = pure (show a)
>
> hetero :: Shade Identity String
> hetero = mconcat [ showshade () , showshade 2 , showshade "hej" ]

The values inside a shade are stored in a context. We can swap this context
by defining a transfer function:

> idToIO :: Identity b -> IO b
> idToIO (Identity v) = putStrLn "Transferring" *> pure v

The context is switched using `transfer` and we can access the value in this
new context by using `shadow`:

> runInIO :: Shade Identity a -> IO a
> runInIO = shadow . transfer idToIO

The point to note about this example is that the values are stored in an
shades with the identity as context. We can manipulate this context
including the value. We cannot, however inspect the value since it is
universally quantified.

> noisy :: String -> Shade Identity (IO ())
> noisy s = pure (putStrLn s)

> main :: IO ()
> main = do
>   r <- runInIO hetero
>   putStrLn r
>   join . runInIO . mconcat . map (noisy . show) $ [0..10]
