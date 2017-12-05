{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad       (zipWithM_)
import           Data.FileEmbed      (embedStringFile)
import qualified Data.Vector.Mutable as MV

jumpHelper :: (Int -> Int) -> Int -> Int -> MV.IOVector Int -> IO Int
jumpHelper changeFunction count pos v = do
  here <- MV.read v pos
  let newPos = pos + here
  MV.modify v changeFunction pos
  if newPos >= MV.length v
    then return (count + 1)
    else jumpHelper changeFunction (count + 1) newPos v

escapeLength :: (Int -> Int) -> [Int] -> IO Int
escapeLength changeFunc ns = do
  v <- MV.new $ length ns
  zipWithM_ (MV.write v) [0 ..] ns
  jumpHelper changeFunc 0 0 v

instructions :: [Int]
instructions = map read $ lines $(embedStringFile "input.txt")

main = do
  putStrLn "Day 05"
  putStr "\tPart A: "
  escapeLength (+ 1) instructions >>= print
  putStr "\tPart B: "
  escapeLength
    (\x ->
       if x >= 3
         then x - 1
         else x + 1)
    instructions >>=
    print

