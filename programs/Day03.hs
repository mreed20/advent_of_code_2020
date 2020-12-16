{-# LANGUAGE OverloadedStrings #-}

module Main where

type Terrain = [String]

-- >>> solve
-- (276,7812180000)
solve :: IO (Int, Int)
solve = do
  t <- readTerrain "inputs/03.txt"
  return (part1 t, part2 t)

part1 :: Terrain -> Int
part1 t = treesEncountered $ makePath t 3

part2 :: Terrain -> Int
part2 t =
  let paths = map (makePath t) [1, 3, 5, 7] -- all the "down 1" paths
      -- Right 1, down 2.
      t' = dropEveryOtherElement t 
      anotherPath = makePath t' 1
   in product . map treesEncountered $ anotherPath : paths

dropEveryOtherElement :: [a] -> [a]
dropEveryOtherElement xs =
  let xs' = zip xs [0 ..]
      xs'' = filter (even . snd) xs'
   in map fst xs''

treesEncountered :: [Char] -> Int
treesEncountered = length . filter (== '#')

-- Starting at the top-left corner of your map and going down by
-- one and right the given amount, how many trees would you encounter?
makePath :: Terrain -> Int -> String
makePath t offset = go t
  where
    go [] = []
    -- add from xs the element at the given offset to the path
    go (xs : xss) = let xss' = map (drop offset) xss
                    in head xs : go xss'

-- cycle makes the terrain repeat to the right forever
readTerrain :: FilePath -> IO Terrain
readTerrain path = do
  s <- readFile path
  return . map cycle . lines $ s