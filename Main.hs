{-# LANGUAGE MultiParamTypeClasses #-}
module Main ( main ) where

import Data.Functor.Identity
import Control.Monad

import Box

idToIO :: Identity b -> IO b
idToIO (Identity v) = putStrLn "Transferring" *> pure v

runInIO :: Box Identity a -> IO a
runInIO = project . transfer idToIO

showbox :: Applicative m => Show a => a -> Box m String
showbox a = pure (show a)

hetero :: Box Identity String
hetero = mconcat [ showbox () , showbox 2 , showbox "hej" ]

noisy :: String -> Box Identity (IO ())
noisy s = pure (putStrLn s)

main :: IO ()
main = do
  r <- runInIO hetero
  putStrLn r
  join . runInIO . mconcat . map (noisy . show) $ [0..10]
