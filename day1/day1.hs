-- Specifically, they need you to find the two entries that sum to 2020 and then
-- multiply those two numbers together.

main :: IO ()
main = do
    numbers <- fileToInts "input"
    case findPairThatSumsTo2020 numbers of
      Just (x, y) -> putStrLn . show $ x * y
      Nothing     -> putStrLn "oh no"

findPairThatSumsTo2020 :: [Int] -> Maybe (Int, Int)
findPairThatSumsTo2020 [] = Nothing
findPairThatSumsTo2020 (x:xs) | y `elem` xs = Just (x, y)
                              | otherwise   = findPairThatSumsTo2020 xs
                              where y = 2020 - x

fileToInts :: FilePath -> IO [Int]
fileToInts path = do
  s <- readFile path
  return $ map read $ lines s