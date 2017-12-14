module Main where

import           Control.Lens (imap, ix, (&), (.~))
import qualified Data.IntMap  as I
import           Data.List    (maximumBy)

data Bank a =
  Bank Int
       (I.IntMap a)
  deriving (Show, Eq)

bankFromList :: [a] -> Bank a
bankFromList xs = Bank (length xs) (I.fromList $ zip [0..] xs)

redistribute :: Bank Int -> Bank Int
redistribute (Bank s im) =
    let compFunc (a,b) (c,d)
            | b == d = compare c a
            | otherwise = compare b d
        (mi, mVal) = maximumBy compFunc $ I.toList im
        delta = mVal `div` s
        elemsToAdd = map (`mod` s) [mi + 1 .. mi + mVal `rem` s]
        helper i x =
            if i `elem` elemsToAdd
                then x + 1 + delta
                else x + delta
    in Bank s $ im & ix mi .~ 0 & imap helper

untilRepeat :: Eq a => (a -> a) -> a -> [a]
untilRepeat f start =
    let helper (x:xs)
            | any (f x ==) (x : xs) = f x : x : xs
            | otherwise = helper (f x : x : xs)
    in helper [start]

lengthToRepeat :: Eq a => [a] -> Int
lengthToRepeat (x:xs) = 1 + length (takeWhile (/= x) xs)

partA :: Bank Int -> Int
partA = length . tail . untilRepeat redistribute

partB :: Bank Int -> Int
partB = lengthToRepeat . untilRepeat redistribute

input :: Bank Int
input = bankFromList [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

main :: IO ()
main = do
  putStrLn "Day 06"
  putStr "\tPart A: "
  print $ partA input
  putStr "\tPart B: "
  print $ partB input
