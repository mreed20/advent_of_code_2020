{-# LANGUAGE OverloadedStrings #-}

-- This was a lot of help:
-- http://jakewheat.github.io/intro_to_parsing/#applicative-style-parsing-code
--
-- This helped since the keys can be unordered.
-- https://massaioli.wordpress.com/2010/09/15/unordered-parsing-in-parsec/

import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Either (rights)
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec as P
  ( char,
    choice,
    count,
    digit,
    many1,
    notFollowedBy,
    oneOf,
    parse,
    spaces,
    string,
    try,
    (<|>),
  )
import Text.Parsec.Perm
import Text.Parsec.Text (Parser)

data Passport = Passport
  { getByr :: Int,
    getIyr :: Int,
    getEyr :: Int,
    getHgt :: Hgt,
    getHcl :: String,
    getEcl :: EyeColor,
    getPid :: String,
    getCid :: Int
  }
  deriving (Show)

-- Height field of passport.
data Hgt = Hgt Int LengthUnit deriving (Show)

-- Unit of measurement used for the "height" field of a passport.
data LengthUnit = Cm | In deriving (Show)

-- Eye color in a passport
data EyeColor = Amb | Blu | Brn | Gry | Grn | Hzl | Oth
  deriving (Show)

main :: IO ()
main = do
  -- Read each passport's text contents onto its own line.
  ss <- readLines "input.txt"
  -- Parse every passport, keeping only those which parsed correctly.
  let passports = rights $ map (P.parse passport "") ss
  let n = length passports
  -- Print the number of parsed passports
  putStrLn $ "part 2: " ++ show n

-- Parse a passport.
passport :: Parser Passport
passport =
  permute $
    Passport <$$> try byr
      <||> try iyr
      <||> try eyr
      <||> try hgt
      <||> try hcl
      <||> try ecl
      <||> try pid
      <|?> (0, try cid)

readLines :: FilePath -> IO [T.Text]
readLines path = do
  s <- T.readFile path
  -- split on two consecutive newlines
  return $ T.splitOn "\n\n" s

-- country code (optional)
cid :: Parser Int
cid = passportKey "cid" positiveNatural

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
byr :: Parser Int
byr = passportKey "byr" (number 1920 2002)

-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
iyr :: Parser Int
iyr = passportKey "iyr" (number 2010 2020)

-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
eyr :: Parser Int
eyr = passportKey "eyr" (number 2020 2030)

-- hgt (Height) - a number followed by either cm or in:
--   - If cm, the number must be at least 150 and at most 193.
--   - If in, the number must be at least 59 and at most 76.
hgt :: Parser Hgt
hgt = do
  lexeme $ string "hgt"
  lexeme $ char ':'
  n <- lexeme positiveNatural
  u <- lexeme lengthUnit
  case u of
    Cm -> guard $ n >= 150 && n <= 193
    In -> guard $ n >= 59 && n <= 76
  return $ Hgt n u
  where
    lengthUnit = (string "cm" $> Cm) <|> (string "in" $> In)

-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
hcl :: Parser String
hcl = passportKey "hcl" hairColor
  where
    hairColor = char '#' *> count 6 (oneOf ['a' .. 'f'] <|> digit)

-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
ecl :: Parser EyeColor
ecl = passportKey "ecl" eyeColor
  where
    eyeColor =
      choice $
        map
          try
          [ string "amb" $> Amb,
            string "blu" $> Blu,
            string "brn" $> Brn,
            string "gry" $> Gry,
            string "grn" $> Grn,
            string "hzl" $> Hzl,
            string "oth" $> Oth
          ]

-- pid (Passport ID) - a nine-digit number, including leading zeroes.
pid :: Parser String
pid = passportKey "pid" nineDigitNumber
  where
    nineDigitNumber = do
      v <- count 9 digit
      notFollowedBy digit
      return v

-- Parse a key-value pair of the form "ks:value" into a value.
passportKey :: String -> Parser a -> Parser a
passportKey ks vp = do
  lexeme $ string ks
  lexeme $ char ':'
  lexeme vp

-- Parse a number in the range [low, high].
number :: Int -> Int -> Parser Int
number low high = do
  n <- positiveNatural
  guard $ n >= low && n <= high
  return n

-- https://stackoverflow.com/questions/10726085/how-do-i-get-parsec-to-let-me-call-read-int
positiveNatural :: Parser Int
positiveNatural = foldl (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

-- Run the given parser, ignoring leading and trailing spaces
lexeme :: Parser a -> Parser a
lexeme p = do
  spaces
  v <- p
  spaces
  return v