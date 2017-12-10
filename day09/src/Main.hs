{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text            as T

data TextGroup
    = Chars String
    | Garbage String
    | Group [TextGroup]
    deriving (Eq, Show)

stripUnused :: String -> String
stripUnused (x:y:xs)
    | x == '!' = stripUnused xs
    | otherwise = x : stripUnused ( y : xs)
stripUnused (x:xs) = x : stripUnused xs
stripUnused [] = []

parseTextGroup =
  let helper :: Parser [TextGroup]
      helper =
        ((Garbage <$> ("<" >> manyTill anyChar ">")) <|>
         (Group <$> ("{" *> helper <* "}")) <|>
         (Chars <$> many (notChar ','))
        ) `sepBy` ","
  in helper

readGroup = either error id . parseOnly parseTextGroup . T.pack . stripUnused

main :: IO ()
main = putStrLn "Hello, Haskell!"

