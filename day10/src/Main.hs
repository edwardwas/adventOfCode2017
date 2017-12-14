{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Control.Lens
import           Data.Bits
import           Data.Char
import           Numeric      (showHex, showIntAtBase)

periodicSlice :: Int -> Int -> Lens' [a] [a]
periodicSlice start l =
    let getter xs = take l $ drop start (xs ++ xs)
        setter xs =
            foldl (\x f -> f x) xs .
            zipWith (\n -> set (ix ((n + start) `mod` (length) xs))) [0 ..]
    in lens getter setter

run :: (Eq a, Num a) => a -> [b] -> [Int] -> [b]
run loops items instructions =
    let helper loops skipSace i xs (l:ls) =
            helper
                loops
                (skipSace + 1)
                ((i + l + skipSace) `mod` (length xs))
                (over (periodicSlice i l) reverse xs)
                ls
        helper loops skipSace i xs []
            | loops == 0 = xs
            | otherwise = helper (loops-1) skipSace i xs instructions
    in helper (loops-1) 0 0 items instructions

makeDenseHash :: Bits a => [a] -> [a]
makeDenseHash [] = []
makeDenseHash xs = helper (take 16 xs) : makeDenseHash (drop 16 xs)
  where helper (y:ys) = foldl xor y ys

input :: [Int]
input = [88, 88, 211, 106, 141, 1, 78, 254, 2, 111, 77, 255, 90, 0, 54, 205]

inputString :: String
inputString = "88,88,211,106,141,1,78,254,2,111,77,255,90,0,54,205"

partA :: [Int] -> Int
partA i =
    let a:b:_ = run 1 [0 .. 255] i
    in a * b

partB :: String -> String
partB str =
    foldl
        (.)
        id
        (map showHex $
         makeDenseHash $
         run 64 [0 :: Int .. 255] $ map ord str ++ [17, 31, 73, 47, 23])
        ""

main :: IO ()
main = do
  putStrLn "Day 10"
  putStr "\tPart A: "
  print $ partA input
  putStr "\tPart B: "
  print $ partB inputString
