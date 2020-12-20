{-# LANGUAGE OverloadedLists #-}

module Day09 where

import Control.Arrow
import Data.Foldable (Foldable (toList))
import Data.Maybe (mapMaybe)
import Data.Sequence
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
import Prelude hiding (drop, filter, length, reverse, take)

type Parser = Parsec Void T.Text

-- >>> solve
-- "(530627549,Nothing)"
solve :: IO String
solve = do
  input <- T.readFile "inputs/09.txt"
  return $ case parse integers "" input of
    Left bundle -> errorBundlePretty bundle
    Right xs ->
      let n = part1 xs
       in -- the answer to part 1 is used in part 2
          show (n, part2 xs n)

part1 :: Seq Int -> Int
part1 xs =
  let hasProperty (xs :|> x) = findPairSum xs x
      groups = groupings 26 xs
      ((_ :|> y) :<| _) = filter (not . hasProperty) groups
   in y

-- Find a contiguous set of at least two numbers in xs which sum to n.
part2 :: Seq Int -> Int -> Maybe (Seq Int)
part2 xs n = go xs
  where
    go a@((y1 :<| ys) :|> y2)
      | sum a == n = Just a
      | sum a - y2 < n = go (ys :|> y2)
    go _ = Nothing

-- >>> let x = groupings 3 (fromList [1,2,3,4,5,6,7,8,9] :: Seq Int) in (Data.Sequence.length x, x)
-- (7,fromList [fromList [1,2,3],fromList [2,3,4],fromList [3,4,5],fromList [4,5,6],fromList [5,6,7],fromList [6,7,8],fromList [7,8,9]])
-- >>> let x = groupings 3 (fromList [1,2,3,4,5,6,7,8,9] :: Seq Int) in (Data.Sequence.length x, x)
-- (7,fromList [fromList [1,2,3],fromList [2,3,4],fromList [3,4,5],fromList [4,5,6],fromList [5,6,7],fromList [6,7,8],fromList [7,8,9]])
groupings ::
  Int {- length of preamble -} ->
  Seq Int {- sequence of numbers -} ->
  Seq (Seq Int)
groupings n seq = fromList [take n (drop i seq) | i <- [0 .. length seq - n]]

findPairSum :: Seq Int -> Int -> Bool
findPairSum [] _ = False
findPairSum (x :<| xs) s
  | y `elem` xs = True
  | otherwise = findPairSum xs s
  where
    y = s - x

{-
-- parsers
-}

integers :: Parser (Seq Int)
integers = fromList <$> (integer `sepBy` C.newline)

-- +90, -5
integer :: Parser Int
integer = L.lexeme C.hspace L.decimal