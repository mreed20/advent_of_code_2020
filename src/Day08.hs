{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
part1 instrs = getAcc $ eval instrs mkEvalState

-- Evaluate the sequence of instructions, stopping before
-- we evaluate an instruction twice.
-- eval :: Array Int Instr -> State EvalState ()
eval :: Array Int Instr -> EvalState -> EvalState
eval instrs s@(EvalState ii acc executed)
  -- Only keep going if the instruction has not been executed already.
  | ii `elem` executed = s
  | otherwise =
    let modifyII f s = s {getII = f (getII s)}
        modifyAcc f s = s {getAcc = f (getAcc s)}
        modifyExecuted f s = s {getExecuted = f (getExecuted s)}
        -- determine the next state
        s' = case instrs ! ii of
          -- Increment the instruction index by one.
          Nop -> modifyII (+ 1) s
          -- Increment the instruction index by one
          -- and the accumulator by the given amount
          Acc inc -> modifyII (+ 1) . modifyAcc (+ inc) $ s
          -- Add off to instruction index.
          Jmp off -> modifyII (+ off) s
     in eval instrs (modifyExecuted (ii :) s')

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