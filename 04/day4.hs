{-# LANGUAGE OverloadedStrings #-}

-- TODO: definitely want to learn how to use Parsec and parser combinators later,
-- since this ad-hoc parsing is painful (damn you Data.Text!)

import qualified Data.Set as S
import qualified Data.Text as T
import Control.Arrow ( (>>>) )

-- key in a passport
data Key
  = Byr
  | Iyr
  | Eyr
  | Hgt
  | Hcl
  | Ecl
  | Pid
  | Cid
  deriving (Eq, Ord, Read, Show)

-- For our purposes a passport is just a set of keys
type Passport = (S.Set Key)

main :: IO ()
main = do
  passports <- readPassports "input.txt"
  putStrLn $ "part 1: " ++ show (part1 passports)

part1 :: [Passport] -> Int
part1 = length . filter passportValid

-- A passport is valid if it contains all possible Key fields
-- except for Cid, which has no impact on a passport's validity.
passportValid :: Passport -> Bool
passportValid p = let requiredKeys = S.fromList [Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid]
                  in requiredKeys `S.isSubsetOf` p

-- Parse a passport string.
parsePassport :: String -> Passport
parsePassport = words >>> map extractKey >>> map pRead >>> S.fromList
  where 
    -- We upper case the first letter so the derived Read instance works.
    pRead :: String -> Key 
    pRead = read . T.unpack . T.toTitle . T.pack
    -- Return just the key part in a string of the form "key:value".
    extractKey :: String -> String
    extractKey = takeWhile (/= ':')

-- Read all passports from the given file path.
readPassports :: FilePath -> IO [Passport]
readPassports path = do
  s <- readFile path
  -- split on two consecutive newlines
  let ss = T.splitOn "\n\n" (T.pack s)
  -- remove newlines within each passport
  let ss' = map (T.unpack . T.replace "\n" " ") ss
  -- Map each String to a Passport, which is just a Set of Keys.
  -- This makes it much more pleasant to process passports than if
  -- they were strings. Inspired by Lexi Lambda's blog post,
  -- "Parse, don't Validate".
  return $ map parsePassport ss'