{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  n <- part1
  print $ "part 1: " ++ show n
  m <- part2
  print $ "part 2: " ++ show m

part1 :: IO Int
part1 = do
  groups <- readGroups "input.txt"
  let sizes = map groupSize groups
  return $ sum sizes

groupSize :: T.Text -> Int
groupSize = S.size . S.delete '\n' . S.fromList . T.unpack

part2 :: IO Int
part2 = do
  groups <- readGroups "input.txt"
  let sizes = map groupSize2 groups
  return $ sum sizes

groupSize2 :: T.Text -> Int
groupSize2 g =
  let answers = map (S.delete '\n' . S.fromList . T.unpack) . T.lines $ g
      answers' = foldl1 S.intersection answers
   in S.size answers'

readGroups :: FilePath -> IO [T.Text]
readGroups path = do
  s <- T.readFile path
  -- split on two consecutive newlines
  return $ T.splitOn "\n\n" s