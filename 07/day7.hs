{-# LANGUAGE OverloadedStrings #-}

module Day7 where

-- TODO: use Text.Parsec.Token

import Data.Char (digitToInt)
import Data.List ( delete )
import Data.Functor (($>))
import Data.Graph (graphFromEdges, path, vertices)
import Data.Maybe (fromJust)
import Text.Parsec
  ( digit,
    lower,
    many1,
    parse,
    sepBy,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.Parsec.String (Parser)

-- A Bag has a color and can contain 1 or more other types of Bags.

type Bag = (String, [String])

main :: IO ()
main = do
  input <- readFile "input.txt"
  -- Finally, I found a good use for mapM! this results
  -- in bags having the type `Either ParseError [Bag]`,
  -- whereas with `map` the type is `[Either ParseError Bag]`.
  case mapM (parse bag "") (lines input) of
    Left x ->
      print x
    Right bags ->
      putStrLn $ "part 1: " ++ show (part1 bags)

-- represent the bags as a graph
part1 :: [Bag] -> Int
part1 bags =
  let bagToEdge (color, childBags) = (color, color, childBags)
      -- build a directed graph from the list of bags
      (graph, _, vertexFromKey) = graphFromEdges (map bagToEdge bags)
      -- makes it more convenient to get keys
      getKey k = fromJust $ vertexFromKey k
      -- can the given key reach the gold vertex?
      canReachGold k = path graph (getKey k) (getKey "shiny gold")
      keys = "shiny gold" `delete` map fst bags 
   in length . filter canReachGold $ keys

bag :: Parser Bag
bag = do
  c <- color
  token "bags"
  token "contain"
  childColors <- (try (string "no other bags") $> []) <|> childBags
  token "."
  return (c, childColors)

childBags :: Parser [String]
childBags =
  ( do
      number
      c <- color
      try (token "bags") <|> try (token "bag")
      return c
  )
    `sepBy` token ","

color :: Parser String
color = do
  c1 <- lexeme $ many1 lower
  c2 <- lexeme $ many1 lower
  return $ c1 ++ " " ++ c2

-- Parse a number in the range [low, high].
number :: Parser Int
number = lexeme positiveNatural
  where
    -- https://stackoverflow.com/questions/10726085/how-do-i-get-parsec-to-let-me-call-read-int
    positiveNatural :: Parser Int
    positiveNatural = foldl (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

-- Parse a string lexeme.
token :: String -> Parser String
token s = lexeme (string s)

-- Run the given parser, ignoring leading and trailing spaces
lexeme :: Parser a -> Parser a
lexeme p = do
  spaces
  v <- p
  spaces
  return v