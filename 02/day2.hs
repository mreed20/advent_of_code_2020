{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import qualified Data.Text as T

-- A password is just a list of characters
type Password = T.Text

-- A password policy.
data PasswordPolicy = PasswordPolicy {
  policyMin :: Int,
  policyMax :: Int,
  policyChar :: Char
}

-- A password entry in the input.
data PasswordEntry = PasswordEntry PasswordPolicy Password


main :: IO ()
main = do
  entries <- readPasswordEntries "input.txt"
  let l1 = length . filter validPasswordPart1 $ entries
  putStrLn $ "part 1: " ++ show l1
  let l2 = length . filter validPasswordPart2 $ entries
  putStrLn $ "part 2: " ++ show l2

validPasswordPart1 :: PasswordEntry -> Bool
validPasswordPart1 (PasswordEntry (PasswordPolicy min max c) password) =
  let n = T.count (T.singleton c) password
  in n >= min && n <= max

xor :: Bool -> Bool -> Bool
xor a b = if a then not b else b

validPasswordPart2 :: PasswordEntry -> Bool
validPasswordPart2 (PasswordEntry (PasswordPolicy i1 i2 c) password) =
  let containsFirst  = T.index password (i1-1) == c
      containsSecond = T.index password (i2-1) == c
  -- exactly one of the positions must contain the password
  in xor containsFirst containsSecond


parseRange :: T.Text -> (Int, Int)
parseRange s = let [low, high] = T.splitOn "-" s
                   textRead = read . T.unpack
                in (textRead low, textRead high)

readPasswordEntries :: FilePath -> IO [PasswordEntry]
readPasswordEntries path = do
  s <- T.readFile path
  return . map textToPasswordEntry . T.lines $ s

-- sample input:
--
--     8-15 f: fffffffsffffffff
textToPasswordEntry :: T.Text -> PasswordEntry
textToPasswordEntry t = let [range, charPart, pw] = T.words t
                            (low, high) = parseRange range
                            char = charPart `T.index` 0
                            policy = PasswordPolicy low high char
                        in PasswordEntry policy pw

