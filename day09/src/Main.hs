{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Monad.Writer
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH

data Group
    = Group [Group]
    | GroupBase
      deriving (Eq,Show)
makeBaseFunctor ''Group

withBracket :: Char -> Char -> String -> String
withBracket start end xs =
    let helper n (c:cs)
            | c == start = c : helper (n + 1) cs
            | c == end && n == 1 = []
            | c == end = c : helper (n - 1) cs
            | otherwise = c : helper n cs
        helper n []
            | n == 1 = []
     in tail $ helper 0 xs

withBracketList :: Char -> Char -> String -> [String]
withBracketList start end (x:xs)
    | x == start =
        let next = withBracket start end (x : xs)
         in next : withBracketList start end (drop (length next + 1) xs)
    | x == ',' || x == ' ' = withBracketList start end xs
    | otherwise = error $ "Bracket list with: " ++ show (map show (x:xs))
withBracketList _ _ [] = []

garbageStripper :: String -> Writer String String
garbageStripper [] = pure []
garbageStripper (x:xs)
  | x == '<' = helper xs
  | otherwise = (x :) <$> garbageStripper xs
    where helper ('!':_:xs) = helper xs
          helper ('>':xs)   = garbageStripper xs
          helper (x:xs)     = tell [x] >> helper xs

coAlgGroup :: String -> GroupF String
coAlgGroup ('{':xs) = GroupF $ withBracketList '{' '}' ('{':xs)
coAlgGroup (',':xs) = coAlgGroup xs
coAlgGroup []       = GroupBaseF

groupScore :: Group -> Int
groupScore =
    let helper n GroupBase  = n
        helper n (Group gs) = n + sum (map (helper (n + 1)) gs)
     in helper 0

partA :: IO Int
partA =
    groupScore . ana coAlgGroup . fst . runWriter . garbageStripper . init <$>
    readFile "input.txt"

partB :: IO Int
partB = length . execWriter . garbageStripper . init <$> readFile "input.txt"

main :: IO ()
main = do
  putStrLn "Day 09"
  putStr "\tPart A: "
  partA >>= print
  putStr "\tPart B: "
  partB >>= print
