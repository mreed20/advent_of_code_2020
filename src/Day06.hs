{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- >>> solve
-- (6273,3254)
solve :: IO (Int, Int)
solve = do
  groups <- readGroups "inputs/06.txt"
  let n = sum $ map groupSize groups
  let m = sum $ map groupSize2 groups
  return (n, m)

groupSize :: T.Text -> Int
groupSize = S.size . S.delete '\n' . S.fromList . T.unpack

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