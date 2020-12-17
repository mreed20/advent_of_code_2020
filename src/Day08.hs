{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Day08 where

import Control.Monad.State.Strict
import Data.Array.IArray
import Data.Functor (($>))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    errorBundlePretty,
    parse,
    sepBy,
    some,
    (<|>),
  )
import qualified Text.Megaparsec as Text.Megaparsec.Error
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, symbol)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void T.Text

-- Instruction to use in the evaluator.
data Instr = Nop | Acc Int | Jmp Int
  deriving (Show, Eq, Ord)

-- Type of state used in the evaluator function.
data EvalState = EvalState
  { getII :: Int, -- instruction index (like an instruction pointer)
    getAcc :: Int, -- accumulator value
    getExecuted :: [Int] -- list of instructions executed so far
  }

-- Create an EvalState where the instruction index points to
-- the first instruction, the accumulator is set to 0, and the
-- list of instructions executed so far is empty.
mkEvalState :: EvalState
mkEvalState = EvalState 0 0 []

-- >>> solve
-- "1563"
solve :: IO String
solve = do
  input <- T.readFile "inputs/08.txt"
  return $ case parse instrs "" input of
    Left bundle -> errorBundlePretty bundle
    Right xs ->
      let arr = listArray (0, length xs - 1) xs
       in show $ part1 arr

part1 :: Array Int Instr -> Int
part1 instrs =
  let finalState = execState (eval instrs) mkEvalState
   in getAcc finalState

-- Evaluate the sequence of instructions, stopping before
-- we evaluate an instruction twice.
eval :: Array Int Instr -> State EvalState ()
eval instrs = do
  -- Only keep going if the instruction has not been executed already.
  --
  -- I'm not 100% clear on wy we need FlexibleContexts here, but I
  -- believe it's because the constraints on modifyII/Acc have concrete
  -- type variables in them: e.g., they are of the form MonadError Failure m
  -- instead of MonadError a m. See here for more info: https://stackoverflow.com/questions/22795768/non-type-variable-argument-in-the-constraint-monaderror-failure-m
  let modifyII fi = modify $ \s -> s { getII = fi (getII s)} 
  let modifyAcc fa = modify $ \s -> s { getAcc = fa (getAcc s)} 
  let modifyExecuted f = modify $ \s -> s { getExecuted = f (getExecuted s)} 
  executed <- gets getExecuted
  ii <- gets getII
  when (ii `notElem` executed) $ do
    -- record this instruction
    modifyExecuted (ii:)
    -- Evaluate the next instruction.
    case instrs ! ii of
      -- Increment the instruction index by one.
      Nop -> modifyII (+1)
      -- Increment the instruction index by one
      -- and the accumulator by the given amount
      Acc inc -> modifyII (+1) *> modifyAcc (+inc)
      -- Add off to instruction index.
      Jmp off -> modifyII (+off)
    -- make recursive call
    eval instrs

-- eval instrs Nop       = (1, 0)
-- eval instrs (Acc inc) = (1, inc)
-- eval instrs (Jmp off) = (off, 0)

{-
-- parsers
-}

instrs :: Parser [Instr]
instrs = instr `sepBy` C.newline

instr :: Parser Instr
instr = nop <|> acc <|> jmp

nop :: Parser Instr
nop = (string "nop" *> offset) $> Nop

acc :: Parser Instr
acc = string "acc" *> (Acc <$> offset)

jmp :: Parser Instr
jmp = string "jmp" *> (Jmp <$> offset)

offset :: Parser Int
offset =
  C.char '+' *> integer
    <|> C.char '-' *> (negate <$> integer)

-- helpers
string :: T.Text -> Parser T.Text
string = L.symbol C.hspace

-- +90, -5
integer :: Parser Int
integer = L.lexeme C.hspace L.decimal