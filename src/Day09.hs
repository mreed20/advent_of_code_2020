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
-- "(530627549,fromList [77730285])"
solve :: IO String
solve = do
  input <- T.readFile "inputs/09.txt"
  return $ case parse integers "" input of
    Left bundle -> errorBundlePretty bundle
    Right xs ->
      let n = part1 xs
       in -- the answer to part 1 is used in part 2
          show (n, part2 n xs)

part1 :: Seq Int -> Int
part1 xs =
  let hasProperty (xs :|> x) = findPairSum xs x
      groups = groupings 26 xs
      ((_ :|> y) :<| _) = filter (not . hasProperty) groups
   in y

-- Find a contiguous set of at least two numbers in xs which sum to n.
-- Note that this is very slow :)
part2 :: Int -> Seq Int -> Seq Int
part2 n = 
  contiguousSubsequences
  >>> filter (\xs -> length xs >= 2)
  >>> filter (\xs -> sum xs == n)
  >>> fmap (\xs -> minimum xs + maximum xs)

-- >>> contiguousSubsequences [1..4]
-- fromList [fromList [1],fromList [1,2],fromList [1,2,3],fromList [1,2,3,4],fromList [2],fromList [2,3],fromList [2,3,4],fromList [3],fromList [3,4],fromList [4]]
contiguousSubsequences :: Seq Int -> Seq (Seq Int)
contiguousSubsequences xs =
  let n = length xs
   in fromList
        [ take (b - a + 1) (drop a xs)
          | a <- [0 .. n - 1],
            b <- [a .. n - 1]
        ]

{-
there are no negative numbers in the sequence
we must find a contiguous range of at least 2 numbers that sums to n
- then we return the min and max in the range (which is not necessarily
  the smallest and largest index)
-}

-- >>> groupings 3 (fromList [1..10])
-- fromList [fromList [1,2,3],fromList [2,3,4],fromList [3,4,5],fromList [4,5,6],fromList [5,6,7],fromList [6,7,8],fromList [7,8,9]]
-- >>> groupings 1 (fromList [1..10])
-- fromList [fromList [1],fromList [2],fromList [3],fromList [4],fromList [5],fromList [6],fromList [7],fromList [8],fromList [9],fromList [10]]
groupings ::
  Int {- length of preamble -} ->
  Seq Int {- sequence of numbers -} ->
  Seq (Seq Int)
groupings n seq = fromFunction (length seq - n + 1) (\i -> take n (drop i seq))

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