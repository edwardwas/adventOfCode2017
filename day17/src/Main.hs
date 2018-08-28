{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Data.List

target =50000000
input = 371

main :: IO ()
main = do
  putStr "Part A: "
  print $ head $ tail $ unCyclicBuffer $ head $ drop 2016 $ applyNSpins input 1 $ CyclicBuffer [] [0]
  putStr "Part B: "
  print $ partB input target

data CyclicBuffer a = CyclicBuffer [a] [a]
  deriving (Eq,Show,Functor,Foldable)

unCyclicBuffer :: CyclicBuffer a -> [a]
unCyclicBuffer (CyclicBuffer ps fs) = fs ++ reverse ps

moveForeward (CyclicBuffer ps (f : fs)) = CyclicBuffer (f : ps) fs
moveForeward (CyclicBuffer ps [])       = moveForeward $ CyclicBuffer [] (reverse ps)

addAfter :: a -> CyclicBuffer a -> CyclicBuffer a
addAfter b (CyclicBuffer ps (f:fs)) = CyclicBuffer ps (f : b : fs)
addAfter b (CyclicBuffer ps []) = addAfter b $ CyclicBuffer [] (reverse ps)

applyNSpins :: Num a => Int -> a -> CyclicBuffer a -> [CyclicBuffer a]
applyNSpins steps n cb =
  let cb' = moveForeward $ addAfter n $ foldr (.) id (replicate steps moveForeward ) cb --iterate moveForeward cb
  in cb' : applyNSpins steps (n + 1) cb'

-- | Part B

partB steps maxInserts =
  let helper (pos, currentOut) i =
        let newPos = (1 + pos + steps) `mod` i
        in if newPos == 0
             then (newPos, Just i)
             else (newPos, currentOut)
  in snd $ foldl' helper (0, Nothing) [1 .. maxInserts]
