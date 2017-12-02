{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Main where

import           Data.Attoparsec.Text
import           Data.FileEmbed
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup       hiding (First, getFirst)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

parseInput :: Parser [[Int]]
parseInput =
    sepBy1 (sepBy1 decimal (many1 (satisfy isHorizontalSpace))) endOfLine

newtype MinMax a =
    MinMax (Min a, Max a)
    deriving (Eq, Show, Ord, Semigroup, Monoid)

minMaxDifference :: Num a => MinMax a -> a
minMaxDifference (MinMax (mi, ma)) = getMax ma - getMin mi

minMaxFromNum :: a -> MinMax a
minMaxFromNum a = MinMax (Min a, Max a)

allPairs []     = []
allPairs (x:xs) = map (x,) xs ++ allPairs xs

evenlyDivides :: Integral a => a -> a -> Maybe a
evenlyDivides a b
    | ma `mod` mi == 0 = Just (ma `div` mi)
    | otherwise = Nothing
  where
    ma = max a b
    mi = min a b

partA :: [[Int]] -> Int
partA = sum . map (minMaxDifference . foldMap minMaxFromNum)

partBHelper :: [Int] -> Int
partBHelper =
    fromJust . getFirst . foldMap (First . uncurry evenlyDivides) . allPairs

partB :: [[Int]] -> Int
partB = sum . map partBHelper

main :: IO ()
main = do
  let Right i = parseOnly parseInput $(embedStringFile "input.txt")
  putStrLn "Day 02"
  putStr $ "\tPart A: "
  print $ partA i
  putStr $ "\tPart B: "
  print $ partB i
