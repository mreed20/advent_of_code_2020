{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Data.Functor (($>))
import Data.Graph (graphFromEdges, path)
import Data.List (delete)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    parse,
    sepBy,
    some,
    (<|>),
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, symbol)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void T.Text

-- A Bag has a color and a list of bag names (and their quantity) it can contain.
type Bag = (T.Text, [(T.Text, Int)])

main :: IO ()
main = do
  input <- T.readFile "input.txt"
  -- Finally, I found a good use for mapM! this results
  -- in bags having the type `Either ParseError [Bag]`,
  -- whereas with `map` the type is `[Either ParseError Bag]`.
  case parse bags "" input of
    Left x ->
      putStrLn . errorBundlePretty $ x
    Right bags -> do
      putStrLn $ "part 1: " ++ show (part1 bags)
      putStrLn $ "part 2: " ++ show (part2 bags)

-- represent the bags as a directed graph
part1 :: [Bag] -> Int
part1 bags =
  let bagToEdge (color, childBags) = (color, color, map fst childBags)
      -- build a directed graph from the list of bags
      (graph, _, vertexFromKey) = graphFromEdges (map bagToEdge bags)
      -- makes it more convenient to get keys
      getKey k = fromJust $ vertexFromKey k
      -- can the given key reach the gold vertex?
      canReachGold k = path graph (getKey k) (getKey "shiny gold")
      keys = "shiny gold" `delete` map fst bags
   in length . filter canReachGold $ keys

part2 :: [Bag] -> Maybe Int
part2 bags = do
  start <- lookup "shiny gold" bags
  follow start bags

-- Wow this took WAY too long to figure out.
follow :: [(T.Text, Int)] -> [Bag] -> Maybe Int
follow [] _ = return 0
follow cs bags = do
  let recurse (name, c) = do
        b <- lookup name bags
        r <- follow b bags
        return $ c + c * r
  x <- mapM recurse cs
  return $ sum x

bags :: Parser [Bag]
bags = bag `sepBy` C.newline

bag :: Parser Bag
bag = do
  c <- lexeme color
  string "bags"
  string "contain"
  childColors <- (string "no other bags" $> []) <|> childBags
  string "."
  return (c, childColors)

childBags :: Parser [(T.Text, Int)]
childBags = childBag `sepBy` string ","

childBag :: Parser (T.Text, Int)
childBag = do
  n <- integer
  c <- color
  string "bags" <|> string "bag"
  return (c, n)

color :: Parser T.Text
color = do
  c1 <- lowerString
  c2 <- lowerString
  return . T.concat $ [c1, " ", c2]

-- helpers
lexeme :: Parser a -> Parser a
lexeme = L.lexeme C.hspace

string :: T.Text -> Parser T.Text
string = L.symbol C.hspace

integer :: Parser Int
integer = lexeme L.decimal

lowerString :: Parser T.Text
lowerString = T.pack <$> (lexeme . some $ C.lowerChar)
