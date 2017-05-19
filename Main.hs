{-# LANGUAGE MultiParamTypeClasses #-}
module Main ( main ) where

import Data.Functor.Identity
import Control.Monad

import Shade

idToIO :: Identity b -> IO b
idToIO (Identity v) = putStrLn "Transferring" *> pure v

runInIO :: Shade Identity a -> IO a
runInIO = shadow . transfer idToIO

showshade :: Applicative m => Show a => a -> Shade m String
showshade a = pure (show a)

hetero :: Shade Identity String
hetero = mconcat [ showshade () , showshade 2 , showshade "hej" ]

noisy :: String -> Shade Identity (IO ())
noisy s = pure (putStrLn s)

main :: IO ()
main = do
  r <- runInIO hetero
  putStrLn r
  join . runInIO . mconcat . map (noisy . show) $ [0..10]
