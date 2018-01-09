{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text hiding (take)
import           Data.FileEmbed
import qualified Data.IntMap          as I

data Action
  = Spin Int
  | Exchange Int
             Int
  | Partner Char
            Char
  deriving (Eq, Show)

type World =  I.IntMap Char

parseInput :: Parser [Action]
parseInput =
  let parseAction =
        (Spin <$ "s" <*> decimal) <|>
        (Exchange <$> ("x" *> decimal) <*> ("/" *> decimal)) <|>
        (Partner <$> ("p" *> anyChar) <*> ("/" *> anyChar))
  in parseAction `sepBy` ","

inputActions :: [Action]
inputActions = either error id $ parseOnly parseInput $(embedStringFile "input.txt")

applyAction :: World -> Action -> World
applyAction w (Spin n) = I.mapKeys (\i -> (i + n) `mod` (length w)) w
applyAction w (Exchange a b) =
  let Just aa = w ^? ix a
      Just ab = w ^? ix b
  in w & (ix a .~ ab) & (ix b .~ aa)
applyAction w (Partner a b) =
  let Just ai = w ^? itraversed . filtered (== a) . asIndex
      Just bi = w ^? itraversed . filtered (== b) . asIndex
  in w & (ix ai .~ b) & (ix bi .~ a)

startWorld :: World
startWorld = I.fromList $ zip [0..] ['a'..'p']

runSingleDance :: World -> World
runSingleDance w = foldl applyAction w inputActions

fastSolution n = I.elems ((iterate runSingleDance startWorld) !! (n `mod` 48))

main :: IO ()
main = do
  putStrLn "Day 16"
  putStr "\tPart A: "
  putStrLn (fastSolution 1)
  putStr "\tPart B: "
  putStrLn (fastSolution (10^9))
