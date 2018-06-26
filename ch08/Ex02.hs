-- While filesystems on Unix are usually case-sensitive (e.g., “G” vs. “g”) in filenames, Windows filesystems are not. 
-- Add a parameter to the globToRegex and matchesGlob functions that allows control over case sensitive matching.

module Ex02 where

import Data.Char
import Text.Regex.Posix ((=~))

data Case = Sensitive | Insensitive

-- |Convert a Glob Expression into a Regular Expression, anchor it to the
--  beginning and end of the line
globToRegex :: String -> Case -> String
globToRegex globex Sensitive = '^' : globToRegex' globex ++ "$"
globToRegex globex Insensitive = '^' : globToRegex' (toLowerCase globex) ++ "$"

-- |Checks if a filename matches a glob pattern by converting that glob pattern
--  to a regular expression and matching using that.
matchesGlob :: FilePath -> String -> Case -> Bool
matchesGlob name pattern casesens = name =~ globToRegex pattern casesens

-- |Internal
--  Find glob specific characters, and convert them to regex specific
--  characters, escapes regex specific characters and verify that character
--  classes are properly terminated
globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

-- |Helper
-- convert a String to lowercase
toLowerCase :: String -> String
toLowerCase = map toLower

-- |Helper
--  Escape regex characters.
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]"

-- |Helper
--  Verify character classes are terminated.
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"
