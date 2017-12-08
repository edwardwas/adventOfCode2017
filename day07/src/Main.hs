{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Applicative
import           Control.Comonad
import           Data.Attoparsec.Text
import           Data.FileEmbed
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust)
import           Data.Monoid
import qualified Data.Text                as T
import qualified Data.Text.IO             as T

eitherError :: Either String a -> a
eitherError = either error id

data Node = Node
    { name   :: String
    , weight :: Int
    } deriving (Eq, Show, Ord)

data Tree a =
    Tree a
         [Tree a]
    deriving (Eq, Show, Functor,Foldable,Traversable)
makeBaseFunctor ''Tree

restrictDepth :: Int -> Tree a -> Tree a
restrictDepth 0 (Tree a _)  = Tree a []
restrictDepth n (Tree a as) = Tree a $ map (restrictDepth (n-1)) as

instance Comonad Tree where
  extract (Tree a _) = a
  duplicate (Tree a xs) = Tree (Tree a xs) $ map duplicate xs
  extend f (Tree a as) = Tree (f $ Tree a as) (map (extend f) as)

prettyPrintTree :: Show a => Tree a -> String
prettyPrintTree =
    let helper n (Tree x xs) =
            unlines
                ((replicate n '\t' ++ "Tree " ++ show x ++ ": ") :
                 map (helper (n + 1)) xs)
    in unlines . filter (/= "") . lines . helper 0

parseNode :: Parser (M.Map String (Node, [String]))
parseNode =
    foldl M.union M.empty <$>
    sepBy
        (do let parseName = manyTill letter space <?> "Parse Name"
            node <- Node <$> parseName <* "(" <*> decimal <* ")"
            neighbours <-
                option [] (" -> " >> sepBy (many letter) (char ',' >> space))
            return $ M.singleton (name node) (node, neighbours))
        endOfLine

testGraph :: Tree Node
testGraph =
    eitherError (parseOnly parseNode $(embedStringFile "test.txt") >>= makeTree)

inputGraph :: Tree Node
inputGraph = eitherError (parseOnly parseNode $(embedStringFile "input.txt") >>= makeTree)

makeTree :: M.Map String (Node, [String]) -> Either String (Tree Node)
makeTree m =
    let root = head $ filter (null . findParents m) $ M.keys m
        findParents m n =
            foldMap
                (\(here, ss) ->
                     if any (== n) ss
                         then [here]
                         else [])
                m
        helper (s :: String) = do
            (Node n w, neigh) <-
                maybe (Left $ "Could not find " ++ s) Right $ M.lookup s m
            Tree (Node n w) <$> mapM helper neigh
    in helper root

totalWeight :: Tree Node -> Int
totalWeight = cata (\(TreeF x xs) -> weight x + sum xs)

allEq (x:xs) = all (x ==) xs && allEq xs
allEq []     = True

main = do
  putStrLn "Day 07"
  putStr "\tPart A: "
  putStrLn $ name $ extract inputGraph
