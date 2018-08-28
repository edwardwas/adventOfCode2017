{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Lens
import           Control.Monad.State
import           Data.Bits
import           Data.Char
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           Numeric

binaryValue :: Integral a => a -> [Bool]
binaryValue =
    let helper x
            | x == 0 = [False]
            | x == 1 = [True]
            | otherwise = odd (x `mod` 2) : helper (x `div` 2)
        pad xs
            | length xs >= 8 = xs
            | otherwise = pad (False : xs)
    in pad . helper

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

knotHash :: String -> [Int]
knotHash b =
    makeDenseHash $ run 64 [0 :: Int .. 255] $ map ord b ++ [17, 31, 73, 47, 23]

connected :: (Ord a, MonadState (S.Set a) m) => (a -> a -> Bool) -> [a] -> a -> m ()
connected conFunc possible current = do
  modify (S.insert current)
  old <- get
  let neigh = filter (\x -> conFunc current x && not (S.member x old)) possible
  mapM_ (connected conFunc possible) neigh

floodFill :: Ord a => (a -> a -> Bool) -> [a] -> [S.Set a]
floodFill _ [] = []
floodFill conFunc (x:xs) =
    let con = execState (connected conFunc xs x) S.empty
    in con : floodFill conFunc (xs L.\\ S.toList con)

grid :: [(Integer,Integer)]
grid =
    let position x y True  = Just (x, y)
        position _ _ False = Nothing
        input = "uugsqrei"
    in concatMap
           (\n ->
                catMaybes $
                zipWith (position n) [0..] $
                concatMap binaryValue $ knotHash (input ++ "-" ++ show n))
           [0 .. 127]

partA :: Int
partA = length grid

distance :: (Eq a, Num a) => (a,a) -> (a,a) -> Bool
distance (a, b) (c, d) = abs (a - c) + abs (b - d) == 1

display =
    let m =
            mconcat $
            mconcat $
            zipWith (\n s -> (\x -> M.singleton x n) <$> S.toList s) [0 ..] $
            floodFill distance grid
        chars =
            concat $ repeat $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['@', '#', '~']
    in map (\x ->
                map
                    (\y ->
                         fromMaybe ' ' $
                         (\i -> chars !! fromIntegral i) <$> M.lookup (x, y) m)
                    [0 .. 127])
           [0 .. 127]

partB = length $ floodFill distance grid

main :: IO ()
main = do
  print partA
  print partB
  writeFile "output.txt" $ unlines display
