{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text            as T

data TextGroup
    = Chars String
    | Group [TextGroup]
    deriving (Eq, Show)

main :: IO ()
main = putStrLn "Hello, Haskell!"

parseTextGroup :: Parser TextGroup
parseTextGroup =
    let helper x xs = ("{" >> ((: xs) <$> parseTextGroup)) <|> (xs <$ "}")
    in Group <$> helper "" []
