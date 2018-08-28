{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Attoparsec.Text hiding (take)
import           Data.FileEmbed
import qualified Data.IntMap          as I
import           Data.Maybe
import qualified Data.Text            as T

parseInput :: Parser (I.IntMap Int)
parseInput =
    let helper = (,) <$> decimal <* ": " <*> decimal
    in I.fromList <$> sepBy helper skipSpace

testInput = either error id $ parseOnly parseInput $(embedStringFile "test.txt")

input = either error id $ parseOnly parseInput $(embedStringFile "input.txt")

positionFunc period x =
    let a = x `mod` ((period - 1) * 2)
    in if a > (period - 1)
           then 2 * (period - 1) - a
           else a

journey d i =
    let helper t = do
            range <- I.lookup t i
            let val = positionFunc range (t + d)
            guard (val == 0)
            return (range * t)
    in sum $ mapMaybe helper [0 .. maximum (I.keys i)]

main :: IO ()
main = putStrLn "Hello, Haskell!"
