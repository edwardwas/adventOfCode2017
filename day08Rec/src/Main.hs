{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Control.Applicative
import           Control.Monad            (guard)
import           Data.Attoparsec.Text
import           Data.FileEmbed           (embedStringFile)
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)

type Register = String

data Comparison n
  = Invert (Comparison n)
  | Eq n
  | LessThan n
  | GreaterThan n
    deriving (Eq,Show,Functor)

applyComp :: (Ord n) => Comparison n -> n -> Bool
applyComp (Invert comp) n   = not $ applyComp comp n
applyComp (Eq x) n          = x == n
applyComp (LessThan x) n    = n < x
applyComp (GreaterThan x) n = n > x

data Instruction n
  = EndOfInstructions
  | Instruction Register
                n
                (Comparison n)
                Register
                (Instruction n)
  deriving (Eq, Show, Functor)
makeBaseFunctor ''Instruction

parseInstruction :: forall n . Integral n => Parser (Instruction n)
parseInstruction =
  let helper = id <$ endOfInput <|> parseTerm
      parseTerm :: Parser (Instruction n -> Instruction n)
      parseTerm = do
                      regName <- many letter <?> "First Register Name"
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
                      skipSpace
                      next <- helper
                      return $
                        (next . Instruction regName (modifyDelta n) (compFunc compNum) targetReg )
   in helper <*> pure EndOfInstructions

testInstructions :: (Integral n) => Instruction n
testInstructions = either error id $ parseOnly parseInstruction $(embedStringFile "test.txt")

inputInstructions :: (Integral n) => Instruction n
inputInstructions = either error id $ parseOnly parseInstruction $(embedStringFile "input.txt")

fillRegisters
  :: (Num n, Ord n)
  => Instruction n -> M.Map Register (n,n)
fillRegisters =
  let helper (InstructionF r delta comp t m) =
        fromMaybe m $ do
          let (tVal,_) = fromMaybe (0,0) $ M.lookup t m
          guard (applyComp comp tVal)
          let (rVal,rMax) = fromMaybe (0,0) $ M.lookup r m
          return $ M.insert r (rVal + delta, max rMax $ rVal + delta) m
      helper _ = M.empty
  in cata helper

run :: Integral n => Instruction n -> (n,n)
run instr =
  let r = fillRegisters instr
  in (maximum $ fst <$> r, maximum $ snd <$> r)

main :: IO ()
main = print $ run inputInstructions
