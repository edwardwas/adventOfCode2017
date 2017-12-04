{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Data.FileEmbed
import           Data.List      (sort)
import qualified Data.Text      as T

allUnique
  :: Eq a
  => [a] -> Bool
allUnique []     = True
allUnique (x:xs) = not (any (== x) xs) && allUnique xs

allUniqueSorted
  :: Ord a
  => [[a]] -> Bool
allUniqueSorted [] = True
allUniqueSorted (x:xs) =
  let helper a b = sort a == sort b
  in not (any (helper x) xs) && allUniqueSorted xs

partA :: T.Text -> Int
partA = length . filter allUnique . map T.words . T.lines

partB :: T.Text -> Int
partB = length . filter allUniqueSorted . map (map T.unpack . T.words) . T.lines

main :: IO ()
main = do
  let i = $(embedStringFile "input.txt")
  putStrLn "Day 04"
  putStr "\tPart A: "
  print $ partA i
  putStr "\tPart B: "
  print $ partB i
