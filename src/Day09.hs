{-# LANGUAGE OverloadedLists #-}

module Day09 where

import Control.Monad.State.Strict
import Data.Array.IArray
import Data.Functor (($>))
import Control.Lens
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
import Data.List (findIndices)
import Data.Either (rights)
import Data.Sequence

type Parser = Parsec Void T.Text

-- >>> solve
solve :: IO String
solve = do
  input <- T.readFile "inputs/09.txt"
  return $ case parse integers "" input of
    Left bundle -> errorBundlePretty bundle
    Right xs -> show $ part1 (fromList xs)

part1 xs = undefined

{-
-- parsers
-}

integers :: Parser [Int]
integers = integer `sepBy` C.newline

-- +90, -5
integer :: Parser Int
integer = L.lexeme C.hspace L.decimal