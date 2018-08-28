{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Data.FileEmbed
import           Data.Monoid
import qualified Data.Text      as T

input = T.filter (/= '\n') $(embedStringFile "input.txt")

data HexCoord =
    HexCoord Int
             Int
             Int
    deriving (Eq, Show)

hexDistance (HexCoord x y z) = (abs x + abs y + abs z) `div` 2

instance Monoid HexCoord where
    mempty = HexCoord 0 0 0
    HexCoord a b c `mappend` HexCoord d e f = HexCoord (a + d) (b + e) (c + f)

parseInput =
    let helper t
            | t == "n" = HexCoord 0 1 (-1)
            | t == "s" = HexCoord 0 (-1) 1
            | t == "ne" = HexCoord 1 0 (-1)
            | t == "sw" = HexCoord (-1) 0 1
            | t == "nw" = HexCoord (-1) 1 0
            | t == "se" = HexCoord 1 (-1) 0
            | otherwise = error $ "Could not parse " ++ T.unpack t
    in map helper . T.splitOn ","

partA = hexDistance . mconcat . parseInput

partB = maximum . map hexDistance . scanl mappend mempty . parseInput

main :: IO ()
main = do
  putStrLn "Day 11"
  putStr "\tPart A:"
  print $ partA input
  putStr "\tPart B:"
  print $ partB input
