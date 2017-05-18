{-# LANGUAGE MultiParamTypeClasses #-}
module Main ( main ) where

import Data.Functor.Identity
import Control.Monad

import Box

instance Transfer Identity IO where
  transfer (Identity v) = putStrLn "Transferring" *> pure v

showbox :: Applicative m => Show a => a -> Box m String
showbox x = box (pure x) show

main :: IO ()
main = do
  r <- unbox hetero
  putStrLn r
  join . unbox . mconcat . map (noisy . show) $ [0..10]
  where
    hetero :: Box Identity String
    hetero = mconcat [ showbox () , showbox 2 , showbox "hej" ]
    noisy :: String -> Box Identity (IO ())
    noisy s = box (pure s) putStrLn
