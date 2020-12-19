{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import Control.Applicative (Alternative ((<|>)))
import Data.Either (rights)
import Data.Functor (($>))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
    ( (<|>), Parsec, parse, errorBundlePretty, sepBy )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Sequence as Seq
import Data.Foldable (Foldable(toList))

type Parser = Parsec Void T.Text

-- Instruction to use in the evaluator.
data Instr = Nop Int | Acc Int | Jmp Int
  deriving (Show, Eq, Ord)

-- Type of state used in the evaluator function.
data EvalState = EvalState
  { getII :: Int, -- instruction index (like an instruction pointer)
    getAcc :: Int -- accumulator value
  }

-- Create an EvalState where the instruction index points to
-- the first instruction and the accumulator is set to 0.
mkEvalState :: EvalState
mkEvalState = EvalState 0 0

-- >>> solve
-- "(Left 1563,[767])"
solve :: IO String
solve = do
  input <- T.readFile "inputs/08.txt"
  return $ case parse instrs "" input of
    Left bundle -> errorBundlePretty bundle
    Right xs -> show (part1 xs, part2 xs)

part1 :: Seq.Seq Instr -> Either Int Int
part1 instrs = eval instrs mkEvalState S.empty

-- Fixing the program involves swapping one nop for a jmp.
-- We first make a list of all instruction sequences, one
-- for every possible nop/jmp swap, then we run eval on
-- each sequence.
part2 :: Seq.Seq Instr -> [Int]
part2 instrs =
  let instrss = permuteInstructions instrs
      results = fmap (\is -> eval is mkEvalState S.empty) instrss
   in rights . toList $ results

permuteInstructions :: Seq.Seq Instr -> Seq.Seq (Seq.Seq Instr)
permuteInstructions instrs =
  let -- we can only swap nop and jmp
      canSwap (Nop _) = True
      canSwap (Jmp _) = True
      canSwap _ = False
      -- does the actual swapping
      swap (Nop x) = Jmp x
      swap (Jmp x) = Nop x
      is = Seq.fromList $ Seq.findIndicesL canSwap instrs
   in -- swap the ith instruction
      fmap (\i -> Seq.adjust swap i instrs) is

-- Evaluate the sequence of instructions. If we encounter
-- an infinite loop, return the Left of the last EvalState.
-- Otherwise return a Right containing the last accumulator value.
eval :: Seq.Seq Instr -> EvalState -> S.Set Int -> Either Int Int
eval instrs s@(EvalState ii acc) executed
  -- We've reached the last instruction.
  | ii == length instrs - 1 = Right acc
  -- Only keep going if the instruction has not been executed already.
  | ii `elem` executed = Left acc
  | otherwise =
    let -- determine the next state
        s' = case instrs `Seq.index` ii of
          -- Increment the instruction index by one.
          Nop _ -> s {getII = ii + 1}
          -- Increment the instruction index by one
          -- and the accumulator by the given amount
          Acc inc -> EvalState (ii + 1) (acc + inc)
          -- Add off to instruction index.
          Jmp off -> s {getII = ii + off}
        executed' = ii `S.insert` executed
     in eval instrs s' executed'

{-
-- parsers
-}

instrs :: Parser (Seq.Seq Instr)
instrs = Seq.fromList <$> (instr `sepBy` C.newline)

instr :: Parser Instr
instr = nop <|> acc <|> jmp

nop :: Parser Instr
nop = string "nop" *> (Nop <$> offset)

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