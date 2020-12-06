{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Text.Read as T

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
  print . length . filter isEntryValid $ entries

isEntryValid :: PasswordEntry -> Bool
isEntryValid (PasswordEntry (PasswordPolicy min max c) password) =
  let n = T.count (T.singleton c) password
  in n >= min && n <= max

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

