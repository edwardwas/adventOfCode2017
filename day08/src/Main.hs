{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Applicative  (empty, many, (<|>))
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Attoparsec.Text
import           Data.FileEmbed       (embedStringFile)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       (Max (..), getMax)
import qualified Data.Text            as T

type Register = String

data Comparison
  = Invert Comparison
  | Eq Int
  | LessThan Int
  | GreaterThan Int
    deriving (Eq,Show)

applyComp :: Comparison -> Int -> Bool
applyComp (Invert comp) n   = not $ applyComp comp n
applyComp (Eq x) n          = x == n
applyComp (LessThan x) n    = n < x
applyComp (GreaterThan x) n = n > x

data Instruction
  = Instruction Register
                Int
                Comparison
                Register
  deriving (Eq, Show)

runInstruction :: (MonadWriter (Max Int) m, MonadState (M.Map Register Int) m) => Instruction -> m ()
runInstruction (Instruction r delta comp targetR) = do
  tVal <- fromMaybe 0 <$> preuse (ix targetR)
  when (applyComp comp tVal) $ do
      rVal <- fromMaybe 0 <$> preuse (ix r)
      let n = rVal + delta
      tell $ Max n
      at r . non 0 += delta

parseInstruction :: Parser [Instruction]
parseInstruction =
  (do regName <- many letter <?> "First Register Name"
      skipSpace
      modifyDelta <- (id <$ "inc" <|> negate <$ "dec") <?> "Modify Delta"
      skipSpace
      n <- signed decimal
      skipSpace >> "if" >> skipSpace
      targetReg <- manyTill letter " "
      compFunc <-
        foldl
          (<|>)
          empty
          [ Invert . Eq <$ try "!="
          , Invert . LessThan <$ try ">="
          , Invert . GreaterThan <$ try "<="
          , GreaterThan <$ try ">"
          , LessThan <$ try "<"
          , Eq <$ try "=="
          ] <?>
        "Comparison Function"
      skipSpace
      compNum <- signed decimal
      return $ Instruction regName (modifyDelta n) (compFunc compNum) targetReg) `sepBy`
  skipSpace

instructions :: [Instruction]
instructions = either error id $ parseOnly parseInstruction $(embedStringFile "input.txt")

partAandB :: [Instruction] -> (Int,Int)
partAandB instr =
  over _2 getMax $
  over _1 (maximum . M.elems) $
  runWriter $ execStateT (mapM_ runInstruction instr) M.empty

main = do
  let (partA,partB) = partAandB instructions
  putStrLn "Day 08"
  putStr "\tPart A: "
  print partA
  putStr "\tPart B: "
  print partB
