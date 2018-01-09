module Main where

import qualified Data.Array.Repa as R

toBinary :: Integral a => a -> [a]
toBinary = map (`mod` 2) . takeWhile (/= 0) . iterate (`div` 2)

generatorA :: Integral a => a -> [a]
generatorA = tail . iterate (\x -> (x * 16807) `mod` 2147483647)

generatorB :: Integral a => a -> [a]
generatorB = tail . iterate (\x -> (x * 48271) `mod` 2147483647)

partA n startA startB =
  length $
  filter id $
  take n $
  zipWith
    (==)
    (map (take 16 . toBinary) $ generatorA startA)
    (map (take 16 . toBinary) $ generatorB startB)

runPartA = partA (4 * 10 ^7)  (116 :: Int) (299 :: Int)

partB n startA startB =
  length $
  filter id $
  take n $
  zipWith
    (==)
    (map (take 16 . toBinary) $ filter ((== 0) . (`mod` 4)) $ generatorA startA)
    (map (take 16 . toBinary) $ filter ((== 0) . (`mod` 8)) $ generatorB startB)

runPartB = partB (5 * 10 ^ 6) (116 :: Int) (299 :: Int)

main :: IO ()
main = do
  putStrLn "Day 15"
  putStr "\tPart A: "
  print $ runPartA
  putStr "\tPart B: "
  print $ runPartB
