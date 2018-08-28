{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import qualified Data.List                as L
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map                 as M
import           Data.Maybe

data Domino a =
    Domino a
           a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Bridge a = [Domino a]

otherDomino n (Domino a b)
  | n == a = Just b
  | n == b = Just a
  | otherwise = Nothing

nextConnection :: (Eq a, Num a) => Bridge a -> a
nextConnection []                = 0
nextConnection (Domino a b : xs) = if a == nextConnection xs then b else a

bridgeStrength :: Num a => Bridge a -> a
bridgeStrength = foldl (\n (Domino a b) -> n + a + b) 0

data Tree a = Tree a [Tree a]
  deriving (Eq,Show,Functor)
makeBaseFunctor ''Tree

growTreeHelper (n, pool) =
    TreeF n $ mapMaybe (\d -> (, L.delete d pool) <$> otherDomino n d) pool

growTree :: (Num a, Eq a) => [Domino a] -> Tree a
growTree pool = ana growTreeHelper (0,pool)

strongestBridge :: (Num a, Ord a) => Tree a -> a
strongestBridge =
    let helper (TreeF x []) = x
        helper (TreeF x xs) = x + maximum xs
    in cata helper

input =
    [ Domino 32 31
    , Domino 2 2
    , Domino 0 43
    , Domino 45 15
    , Domino 33 24
    , Domino 20 20
    , Domino 14 42
    , Domino 2 35
    , Domino 50 27
    , Domino 2 17
    , Domino 5 45
    , Domino 3 14
    , Domino 26 1
    , Domino 33 38
    , Domino 29 6
    , Domino 50 32
    , Domino 9 48
    , Domino 36 34
    , Domino 33 50
    , Domino 37 35
    , Domino 12 12
    , Domino 26 13
    , Domino 19 4
    , Domino 5 5
    , Domino 14 46
    , Domino 17 29
    , Domino 45 43
    , Domino 5 0
    , Domino 18 18
    , Domino 41 22
    , Domino 50 3
    , Domino 4 4
    , Domino 17 1
    , Domino 40 7
    , Domino 19 0
    , Domino 33 7
    , Domino 22 48
    , Domino 9 14
    , Domino 50 43
    , Domino 26 29
    , Domino 19 33
    , Domino 46 31
    , Domino 3 16
    , Domino 29 46
    , Domino 16 0
    , Domino 34 17
    , Domino 31 7
    , Domino 5 27
    , Domino 7 4
    , Domino 49 49
    , Domino 14 21
    , Domino 50 9
    , Domino 14 44
    , Domino 29 29
    , Domino 13 38
    , Domino 31 11
    ]


main :: IO ()
main = putStrLn "Hello, Haskell!"
