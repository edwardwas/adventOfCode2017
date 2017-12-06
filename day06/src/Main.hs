{-# LANGUAGE DeriveTraversable #-}

module Main where

import           Control.Lens
import qualified Data.IntMap  as I

data Bank a =
  Bank Int
       (I.IntMap a)
  deriving (Functor, Foldable, Traversable, Show, Eq)

redistribute (Bank s im) =
  let Just (mi, mVal) = imaximumOf itraversed im
      toAdd x = x `elem` map (`mod` s) [mi + 1 .. mi + 1 + mVal `rem` s]
      helper i x = if toAdd i then x + 1 + mVal `mod` s else x + mVal `mod` s
  in im & ix mi .~ 0 & imap helper

main :: IO ()
main = putStrLn "Hello, Haskell!"
