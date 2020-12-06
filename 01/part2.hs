-- Specifically, they need you to find the two entries that sum to 2020 and then
-- multiply those two numbers together.

main :: IO ()
main = do
  numbers <- fileToInts "input"
  case findTriplet numbers 2020 of
    Just (x, y, z) -> print $ x * y * z
    Nothing -> putStrLn "oh no"

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