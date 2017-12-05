{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Data.FileEmbed
import qualified Data.Vector           as V
import           GHC.Exts

instructions :: Focused Int
instructions = fromList $ map read $ lines $(embedStringFile "input.txt")

data Focused a =
  Focused Int
          (V.Vector a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance IsList (Focused a) where
  type Item (Focused a) = a
  toList (Focused _ v) = toList v
  fromList xs = Focused 0 $ V.fromList xs

currentFocused :: Lens' (Focused a) a
currentFocused =
  lens
    (\(Focused n v) -> v V.! n)
    (\(Focused n v) a -> Focused n $ v & ix n .~ a)

instance Comonad Focused where
  extract (Focused n v) = v V.! n
  duplicate (Focused n v) =
    Focused n $ V.fromList $ map (\n' -> Focused n' v) [0 .. V.length v]

instance ComonadStore Int Focused where
  pos (Focused n _) = n
  peek n (Focused _ v) = v V.! n

handleJump :: (Int -> Int) -> Int -> Focused Int -> Int
handleJump changeFunc count v
  | length v <= pos v + extract v = count + 1
  | otherwise =
    handleJump changeFunc (count + 1) $ seeks (+ extract v) $ v & currentFocused %~ changeFunc

main :: IO ()
main = do
  putStrLn "Day 05"
  putStr "\tPart A: "
  print $ handleJump (+ 1) 0 instructions
  putStr "\tPart B: "
  print $ handleJump (\x -> if x >= 3 then x - 1 else x + 1) 0 instructions
