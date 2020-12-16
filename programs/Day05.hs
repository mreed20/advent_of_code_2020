module Main where

import qualified Data.Set as S
import Control.Applicative
import Control.Monad (guard)

-- >>> solve
-- Just (858,557)
solve :: IO (Maybe (Int, Int))
solve = do
  input <- lines <$> readFile "inputs/05.txt"
  let n = part1 input
      m = part2 input
  return (liftA2 (,) n m)

part1 :: [String] -> Maybe Int
part1 = maximum . map stringToSeatId

part2 :: [String] -> Maybe Int
part2 xss = do
  yss <- mapM stringToSeatId xss
  let seats = findEmptySeats yss
  guard $ S.size seats == 1
  return . head . S.toList $ seats

findEmptySeats :: [Int] -> S.Set Int
findEmptySeats actual = expected `S.difference` S.fromList actual
  where actual' = S.fromList actual
        low  = S.findMin actual'
        high = S.findMax actual'
        expected = S.fromList [low..high]

bitsToInt :: [Int] -> Int
bitsToInt bits = sum $ zipWith (*) bits powersOfTwo
                 where powersOfTwo = [2^x | x <- [0..]]

stringToSeatId :: String -> Maybe Int
stringToSeatId xs = do
  bits <- mapM f xs
  return . bitsToInt . reverse $ bits
  where
    f 'F' = Just 0
    f 'B' = Just 1
    f 'L' = Just 0
    f 'R' = Just 1
    f _   = Nothing