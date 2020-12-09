#!/usr/bin/env runhaskell

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brainfuck where

import Control.Lens
    ( (^?), (^.), over, set, makeLenses, At(at), Ixed(ix) )
import qualified Data.Map.Strict as M
import Text.Parsec.ByteString (Parser)
import Text.Parsec (parse, spaces,  oneOf, eof, many1 )
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import Control.Monad.State.Lazy (evalState, modify, get, MonadState, State)
import Data.Maybe (fromMaybe)
import Control.Monad.Writer.Lazy (execWriterT, tell, WriterT, MonadWriter)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

data Instruction = PointerIncrement
                 | PointerDecrement
                 | Increment
                 | Decrement
                 | Output
                 | StartLoop
                 | EndLoop
                 deriving (Show, Eq)

data Status = Run | Halt
            deriving (Show, Eq)

type Program = V.Vector Instruction
type Memory  = M.Map Int Char
type Output  = T.Text

data VMState = VMState { _pc       :: Int
                       , _pointer  :: Int
                       , _loopLoc  :: Maybe Int
                       , _memory   :: Memory
                       , _program  :: Program
                       , _status   :: Status }
             deriving (Show)

$(makeLenses ''VMState)

newtype VM a = VM { unVM :: WriterT Output (State VMState) a }
  deriving (Functor, Applicative, Monad, MonadState VMState, MonadWriter Output)

-- Parser

instruction :: Parser Instruction
instruction = do
  c <- oneOf "><+-,.[]" <* spaces
  case c of
    '>' -> return PointerIncrement
    '<' -> return PointerDecrement
    '+' -> return Increment
    '-' -> return Decrement
    '.' -> return Output
    '[' -> return StartLoop
    ']' -> return EndLoop

assembly :: Parser Program
assembly = do
  prog <- many1 instruction <* eof
  return $ V.fromList prog

-- Pure functions

initVMState :: Program -> VMState
initVMState prog = VMState { _pc      = 0
                           , _pointer = 0
                           , _loopLoc = Nothing
                           , _memory  = M.empty
                           , _program = prog
                           , _status  = Run }

elemIndexFrom :: (Eq a) => Int -> a -> V.Vector a -> Maybe Int
elemIndexFrom from needle vector = (+ from) <$> (V.elemIndex needle . V.drop from) vector

getMem :: VMState -> Char
getMem state = fromMaybe '\0' $ state^.memory.at (state^.pointer)


putMem :: Char -> VMState -> VMState
putMem x state = set (memory.at (state^.pointer)) (Just x) state

runVM :: Program -> Output
runVM prog = evalState (execWriterT (unVM vmExecute)) (initVMState prog)

-- VM actions

vmHalt :: VM ()
vmHalt = modify (set status Halt)

vmPointerInc :: VM ()
vmPointerInc = modify (over pc succ . over pointer succ)

vmPointerDec :: VM ()
vmPointerDec = modify (over pc succ . over pointer pred)

vmArithmetic :: (Char -> Char) -> VM ()
vmArithmetic p = do
  state <- get
  modify $ putMem (p (getMem state))
  modify $ over pc succ

vmIncrement :: VM ()
vmIncrement = vmArithmetic succ'
  where succ' '\255' = '\0'
        succ' x      = succ x

vmDecrement :: VM ()
vmDecrement = vmArithmetic pred'
  where pred' '\0' = '\255'
        pred' x    = pred x

vmStartLoop :: VM ()
vmStartLoop = do
  state <- get

  let char = getMem state
      endLoop' = elemIndexFrom (state^.pc) EndLoop (state^.program)

  if char == '\0' then do
    case endLoop' of
      Just endLoop -> modify $ set pc (succ endLoop)
      Nothing      -> error ("No end loop found (pc=" ++ show (state^.pc) ++ ")")
  else modify $ over pc succ . set loopLoc (Just (state^.pc))

vmEndLoop :: VM ()
vmEndLoop = do
  state <- get

  let char = getMem state

  if char /= '\0' then do
    case state^.loopLoc of
      Just startLoop -> modify $ set pc (succ startLoop)
      Nothing        -> error ("Start loop was not set (pc=" ++ show (state^.pc) ++ ")")
  else modify $ over pc succ . set loopLoc Nothing

vmOutput :: VM ()
vmOutput = do
  state <- get
  let char = getMem state
  tell $ T.singleton char
  modify $ over pc succ

vmExecute :: VM ()
vmExecute = do
  state <- get

  let ins = state^.program^?ix (state^.pc)

  case ins of
    Just PointerIncrement -> vmPointerInc
    Just PointerDecrement -> vmPointerDec
    Just Increment        -> vmIncrement
    Just Decrement        -> vmDecrement
    Just Output           -> vmOutput
    Just StartLoop        -> vmStartLoop
    Just EndLoop          -> vmEndLoop
    Nothing               -> vmHalt

  state <- get

  case state^.status of
    Run  -> vmExecute
    Halt -> return ()

-- IO actions

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn ("Usage: " <> progName <> " <script>")

main :: IO ()
main = do
  args <- getArgs

  if length args /= 1 then usage
  else do
    let fileName = head args

    fileExist <- doesFileExist fileName

    if not fileExist then do
      putStrLn "Script not found"
    else do
      content <- BS.readFile fileName

      let res = parse assembly fileName content

      case res of
        Left  err  -> print err
        Right prog -> TIO.putStrLn $ runVM prog
