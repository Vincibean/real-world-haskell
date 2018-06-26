-- Write a version of globToRegex that uses the type signature shown earlier.

import Data.Char
import Text.Regex.Posix ((=~))

type GlobError = String

-- |Convert a Glob Expression into a Regular Expression, anchor it to the
--  beginning and end of the line
globToRegex :: String -> Either GlobError String
globToRegex globex = fmap ('^' : ) (globToRegex' $ globex ++ "$")

-- |Internal
--  Find glob specific characters, and convert them to regex specific
--  characters, escapes regex specific characters and verify that character
--  classes are properly terminated
globToRegex' :: String -> Either GlobError String
globToRegex' "" = Right ""
globToRegex' ('*':cs) = fmap (".*" ++ ) (globToRegex' cs)
globToRegex' ('?':cs) = fmap ('.' : ) (globToRegex' cs)
globToRegex' ('[':'!':c:cs) = fmap (\x -> "[^" ++ c : x) (charClass cs)
globToRegex' ('[':c:cs) = fmap (\x -> '[' : c : x) (charClass cs)
globToRegex' ('[':_) = Left "unterminated character class"
globToRegex' (c:cs) = fmap (escape c ++ ) (globToRegex' cs)

-- |Helper
--  Escape regex characters.
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]"

-- |Helper
--  Verify character classes are terminated.
charClass :: String -> Either GlobError String
charClass (']':cs) = fmap (']' : ) (globToRegex' cs)
charClass (c:cs)   = fmap (c : ) (charClass cs)
charClass []       = Left "unterminated character class"
