import qualified Data.Set as S

main :: IO ()
main = do
  n <- part1
  print $ "part 1: " ++ show n
  m <- part2
  print $ "part 2: " ++ show m

part1 :: IO Int
part1 = do
  input <- readFile "input.txt"
  return . maximum . map stringToSeatId . lines $ input

part2 :: IO (S.Set Int)
part2 = do
  input <- readFile "input.txt"
  return . findEmptySeats . map stringToSeatId . lines $ input

findEmptySeats :: [Int] -> S.Set Int
findEmptySeats actual = expected `S.difference` S.fromList actual
  where actual' = S.fromList actual
        low  = S.findMin actual'
        high = S.findMax actual'
        expected = S.fromList [low..high]

bitsToInt :: [Int] -> Int
bitsToInt bits = sum $ zipWith (*) bits powersOfTwo
                 where powersOfTwo = [2^x | x <- [0..]]

stringToSeatId :: String -> Int
stringToSeatId = bitsToInt . reverse . map f
  where
    f 'F' = 0
    f 'B' = 1
    f 'L' = 0
    f 'R' = 1