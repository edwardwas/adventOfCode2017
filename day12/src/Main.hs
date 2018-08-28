{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.FileEmbed
import qualified Data.Map             as M
import qualified Data.Set             as S

parseInput :: Parser (M.Map Int (S.Set Int))
parseInput =
    let line = do
            n <- decimal
            skipSpace >> "<->" >> skipSpace
            ns <- decimal `sepBy` ", "
            return $
                foldl (M.unionWith mappend) (M.singleton n $ S.fromList ns) $
                map (\x -> M.singleton x (S.singleton n)) ns
    in foldl (M.unionWith mappend) M.empty <$> line `sepBy` skipSpace

testInput :: M.Map Int (S.Set Int)
testInput =
    either error id $ parseOnly parseInput $(embedStringFile "test.txt")

input :: M.Map Int (S.Set Int)
input = either error id $ parseOnly parseInput $(embedStringFile "input.txt")

floodFill :: M.Map Int (S.Set Int) -> Int -> State (S.Set Int) ()
floodFill m current = do
    let Just neigh = M.lookup current m
    modify (S.insert current)
    nodes <- get
    mapM_ (floodFill m) $
        filter (\x -> not (x `S.member` nodes)) $ S.toList neigh

partA :: M.Map Int (S.Set Int) -> Int
partA i = length $ execState (floodFill i 0) S.empty

partB :: M.Map Int (S.Set Int) -> Int
partB i =
    let helper s =
            case S.minView s of
                Just (x, _) ->
                    let ns = execState (floodFill i x) S.empty
                    in ns : helper (s `S.difference` ns)
                Nothing -> []
    in length $ helper $ S.fromList $ M.keys i


main :: IO ()
main = do
  putStrLn "Day 12"
  putStr "\tPart A: "
  print $ partA input
  putStr "\tPart B: "
  print $ partB input
