{-# LANGUAGE MultiParamTypeClasses #-}
module Main ( main ) where

import Data.Functor.Identity
import Control.Monad

import Box

instance Transfer Identity IO where
  transfer (Identity v) = putStrLn "Transferring" *> pure v

main :: IO ()
main = do
  r <- unbox hetero
  putStrLn r
  join . unbox . mconcat . map (noisy . show) $ [0..10]
  where
    f x = box (pure x) show
    hetero :: Box Identity String
    hetero = mconcat [ f () , f 2 , f "hej" ]
    noisy :: String -> Box Identity (IO ())
    noisy s = box (pure s) putStrLn
