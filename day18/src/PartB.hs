{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module PartB where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Attoparsec.Text
import           Data.FileEmbed
import qualified Data.Map                   as M
import qualified Data.Vector                as V

type Register = Char

data RegisterOrNum
    = R Register
    | N Int
    deriving (Eq, Show)

data Instruction
    = ApplyToRegister (Int -> Int -> Int)
                      Register
                      RegisterOrNum
    | JumpGreaterThanZero RegisterOrNum
                          RegisterOrNum
    | Send RegisterOrNum
    | Recieve Register

parseInstructions :: Parser (V.Vector Instruction)
parseInstructions =
    let parseRegisterOrNum = (R <$> letter) <|> (N <$> signed decimal)
        parseLine =
            foldl
                (<|>)
                empty
                [ Send <$> ("snd " *> parseRegisterOrNum)
                , ApplyToRegister const <$> ("set " *> letter) <*>
                  (" " *> parseRegisterOrNum)
                , ApplyToRegister (+) <$> ("add " *> letter) <*>
                  (" " *> parseRegisterOrNum)
                , ApplyToRegister (*) <$> ("mul " *> letter) <*>
                  (" " *> parseRegisterOrNum)
                , ApplyToRegister mod <$> ("mod " *> letter) <*>
                  (" " *> parseRegisterOrNum)
                , Recieve <$> ("rcv " *> letter)
                , JumpGreaterThanZero <$> ("jgz " *> parseRegisterOrNum) <*>
                  (" " *> parseRegisterOrNum)
                ]
    in V.fromList <$> (parseLine `sepBy` endOfLine)

instructions :: V.Vector Instruction
instructions =
    either error id $ parseOnly parseInstructions $(embedStringFile "input.txt")

data ProgramState = ProgramState
    { _programId   :: Int
    , _registers   :: M.Map Register Int
    , _sendCounter :: Int
    } deriving (Eq, Show)
makeLenses ''ProgramState

getRegisterOrNum :: MonadState ProgramState m => RegisterOrNum -> m Int
getRegisterOrNum (R r) = do
  defValue <- if r == 'p' then use programId else return 0
  use (registers . at r . non defValue)
getRegisterOrNum (N n) = return n

test = [Send $ N 1, Send $ N 2, Send $ R 'p', Recieve 'a', Recieve 'b', Recieve 'c', Recieve 'd']

runInstruction :: TQueue  Int -> Instruction -> StateT (ProgramState) IO (Int -> Int)
runInstruction _ (ApplyToRegister f rx ry) = do
  v <- f <$> getRegisterOrNum (R rx) <*> getRegisterOrNum ry
  registers . at rx .= Just v
  return succ
runInstruction _ (JumpGreaterThanZero rx ry) = do
  r <- getRegisterOrNum rx
  if r == 0 then return succ else (+) <$> getRegisterOrNum ry
runInstruction chan (Send ron) = do
  sendCounter += 1
  val <- getRegisterOrNum ron
  liftIO $ atomically $ writeTQueue chan val
  return succ
runInstruction chan (Recieve r) = do
  val <- liftIO $ atomically $ readTQueue chan
  registers . at r .= Just val
  return succ

runPrograms instr = do
  chan0 <- newTQueueIO
  chan1 <- newTQueueIO
  return ()
