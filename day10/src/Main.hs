module Main where

import           Control.Lens

periodicSlice start l f xs =
  let h = drop start (xs ++ xs)
      m = take l h
      e = drop l h
  in (\m' -> take (length xs) $ h ++ m' ++ e) <$> f m

main :: IO ()
main = putStrLn "Hello, Haskell!"
