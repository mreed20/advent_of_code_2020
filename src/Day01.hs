-- Specifically, they need you to find the two entries that sum to 2020 and then
-- multiply those two numbers together.

module Day01 where

import Control.Applicative (Applicative (liftA2))

-- >>> solve
-- Just (276432018,1016131)
solve :: IO (Maybe (Int, Int))
solve = do
  numbers <- fileToInts "inputs/01.txt"
  let m = uncurry (*) <$> findPairThatSumsToSomething numbers 2020
  let n = product3 <$> findTriplet numbers 2020
  return $ liftA2 (,) n m

product3 :: Num a => (a, a, a) -> a
product3 (x, y, z) = x * y * z

findTriplet :: [Int] -> Int -> Maybe (Int, Int, Int)
findTriplet [] _ = Nothing
findTriplet (x : xs) s =
  case findPairThatSumsToSomething xs (s - x) of
    Nothing -> findTriplet xs s
    Just (y, z) -> Just (x, y, z)

findPairThatSumsToSomething :: [Int] -> Int -> Maybe (Int, Int)
findPairThatSumsToSomething [] _ = Nothing
findPairThatSumsToSomething (x : xs) s
  | y `elem` xs = Just (x, y)
  | otherwise = findPairThatSumsToSomething xs s
  where
    y = s - x

fileToInts :: FilePath -> IO [Int]
fileToInts path = do
  s <- readFile path
  return $ map read $ lines s
