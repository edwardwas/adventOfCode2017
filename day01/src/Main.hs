{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Data.Char      (isNumber)
import           Data.FileEmbed

readInput :: IO [Int]
readInput = map (read . pure) . filter isNumber <$> readFile "input.txt"

partA :: (Num a, Eq a) => [a] -> a
partA [] = 0
partA (x:xs) = sum $ map fst $ filter (uncurry (==)) $ zip (x : xs) (xs ++ [x])

partB :: (Num a, Eq a) => [a] -> a
partB [] = 0
partB xs =
  let l = length xs
      helper n = xs !! ((n + l `div` 2) `mod` l)
  in sum $ zipWith (\n x -> if helper n == x then x else 0) [0..] xs

main :: IO ()
main = do
  let i = map (read . pure) $ filter isNumber $(embedStringFile "input.txt")
  putStrLn "Day One"
  putStr $ "\tPart A: "
  print $ partA i
  putStr $ "\tPart B: "
  print $ partB i
