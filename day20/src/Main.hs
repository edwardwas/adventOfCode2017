{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Main where

import           Control.Applicative  (liftA2)
import           Control.Monad
import           Data.Attoparsec.Text hiding (take)
import           Data.FileEmbed
import           Data.Function        (on)
import qualified Data.List            as L
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Text            as T
import           Linear


manhattanDistance :: Num a => V3 a -> V3 a -> a
manhattanDistance x = foldl (+) 0 . liftA2 (\a b -> abs (a - b)) x

data Particle a = Particle
    { position     :: V3 a
    , velocity     :: V3 a
    , acceleration :: V3 a
    } deriving (Eq, Show, Functor)

parseInput :: Integral a => Parser [Particle a]
parseInput =
    let parseLine = do
            "p=<"
            x <- signed decimal <* ","
            y <- signed decimal <* ","
            z <- signed decimal <* ">, v=<"
            vx <- signed decimal <* ","
            vy <- signed decimal <* ","
            vz <- signed decimal <* ">, a=<"
            ax <- signed decimal <* ","
            ay <- signed decimal <* ","
            az <- signed decimal <* ">"
            return $ Particle (V3 x y z) (V3 vx vy vz) (V3 ax ay az)
    in parseLine `sepBy` space

input :: Integral a => [Particle a]
input = either error id $ parseOnly parseInput $(embedStringFile "input.txt")

testInput :: Integral a => [Particle a]
testInput = either error id $ parseOnly parseInput $(embedStringFile "test.txt")

partA :: Int
partA =
    fst $
    head $
    L.sortBy
        (mappend
             (compare `on` (manhattanDistance (pure 0) . acceleration . snd))
             (compare `on` (manhattanDistance (pure 0) . velocity . snd))) $
    zip [0 ..] input

type World = [Particle Int]

positionAtTime :: Integral a => a -> Particle a -> V3 a
positionAtTime t (Particle p v a)
  | t == 0 = p
  | otherwise = positionAtTime (t - 1) $ Particle (p + v + a) (v + a) a

collisionTime :: Integral a => Particle a -> Particle a -> Maybe a
collisionTime p1 p2 =
    let helper t
            | positionAtTime t p1 == positionAtTime t p2 = Just t
            | manhattanDistance (positionAtTime t p1) (positionAtTime t p2) <=
                  manhattanDistance
                      (positionAtTime (t + 1) p1)
                      (positionAtTime (t + 1) p2) = Nothing
            | otherwise = helper (t + 1)
    in if p1 == p2 then Nothing else helper 0

nextCollsions :: World -> World
nextCollsions w
    | null endTimes = w
    | otherwise =
        catMaybes $
        zipWith
            (\n p ->
                 if n `elem` timeToRemove
                     then Nothing
                     else Just p)
            [0 ..]
            w
  where
    helper x = getMin <$> foldMap (fmap Min . collisionTime x) w
    endTimes = catMaybes $ zipWith (\n -> fmap (n, )) [0 ..] $ map helper w
    firstCollision = minimum $ map snd endTimes
    timeToRemove =
        mapMaybe
            (\(n, x) ->
                 if x /= firstCollision
                     then Nothing
                     else Just n)
            endTimes

partBLooper w = do
  print $ length w
  unless (nextCollsions w == w) $ partBLooper $ nextCollsions w

main :: IO ()
main = partBLooper input
