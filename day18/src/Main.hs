{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           PartA
import           PartB

main = do
  putStrLn "Day 18"
  putStr "\tPart A: "
  print $ partA
  putStr "\tPart B: "

