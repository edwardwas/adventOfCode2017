{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module PartA
    ( partA
    ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Attoparsec.Text
import           Data.FileEmbed
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Vector          as V

type Register = Char

data RegisterOrNum
    = R Register
    | N Int
    deriving (Eq, Show)

data Instruction
    = PlaySound RegisterOrNum
    | SetRegister Register
                  RegisterOrNum
    | AddRegister Register
                  RegisterOrNum
    | MulRegister Register
                  RegisterOrNum
    | ModRegister Register
                  RegisterOrNum
    | RecoverLastSound RegisterOrNum
    | Jump RegisterOrNum
           RegisterOrNum
      deriving (Eq,Show)

parseInstructions :: Parser (V.Vector Instruction)
parseInstructions =
    let parseRegisterOrNum = (R <$> letter) <|> (N <$> signed decimal)
        parseLine =
            (PlaySound <$> ("snd " *> parseRegisterOrNum)) <|>
            (SetRegister <$> ("set " *> letter) <*> (" " *> parseRegisterOrNum)) <|>
            (AddRegister <$> ("add " *> letter) <*> (" " *> parseRegisterOrNum)) <|>
            (MulRegister <$> ("mul " *> letter) <*> (" " *> parseRegisterOrNum)) <|>
            (ModRegister <$> ("mod " *> letter) <*> (" " *> parseRegisterOrNum)) <|>
            (RecoverLastSound <$> ("rcv " *> parseRegisterOrNum)) <|>
            (Jump <$> ("jgz " *> parseRegisterOrNum) <*>
             (" " *> parseRegisterOrNum))
    in V.fromList <$> (parseLine `sepBy` endOfLine)

testInstructions :: V.Vector Instruction
testInstructions =
    either error id $
    parseOnly parseInstructions ($(embedStringFile "test.txt") :: T.Text)

instructions :: V.Vector Instruction
instructions =
    either error id $
    parseOnly parseInstructions ($(embedStringFile "input.txt") :: T.Text)

data ProgramState = ProgramState
    { _registers :: M.Map Register Int
    , _lastSound :: Int
    } deriving (Eq, Show)

makeLenses ''ProgramState

getRegisterOrNum :: MonadState ProgramState m => RegisterOrNum -> m Int
getRegisterOrNum (R r) = do
      v <- preuse (registers . ix r)
      case v of
          Just x  -> return x
          Nothing -> return 0
getRegisterOrNum (N n) = return n

runInstruction ::
       MonadState ProgramState m => Instruction -> m (Either Int (Int -> Int))
runInstruction (PlaySound ron) =
    Right succ <$ (getRegisterOrNum ron >>= assign lastSound)
runInstruction (SetRegister r ron) =
    Right succ <$ (getRegisterOrNum ron >>= assign (registers . at r) . Just)
runInstruction (AddRegister r ron) =
    Right succ <$ (getRegisterOrNum ron >>= modifying (registers . ix r) . (+))
runInstruction (MulRegister r ron) =
    Right succ <$ (getRegisterOrNum ron >>= modifying (registers . ix r) . (*))
runInstruction (ModRegister r ron) =
    Right succ <$
    (getRegisterOrNum ron >>= \y ->
         modifying (registers . ix r) (\x -> x `mod` y))
runInstruction (RecoverLastSound ron) = do
    rVal <- getRegisterOrNum ron
    if rVal == 0
        then return (Right succ)
        else Left <$> use lastSound
runInstruction (Jump x y) = do
    xVal <- getRegisterOrNum x
    if xVal == 0
        then return (Right succ)
        else do
            yVal <- getRegisterOrNum y
            return $ Right (+ yVal)

runUntilFirstSound v =
    let helper n
          | n >= V.length v = return Nothing
          | otherwise = do
                      delta <- runInstruction (v V.! n)
                      case delta of
                        Right f -> helper (f n)
                        Left x  -> return $ Just x
    in runState (helper 0) (ProgramState M.empty (-1))

partA = fst $ runUntilFirstSound instructions
