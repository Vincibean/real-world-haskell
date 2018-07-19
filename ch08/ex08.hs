-- Glob patterns are simple enough to interpret that it’s easy to write a matcher directly in Haskell, 
-- rather than going through the regexp machinery. Give it a try.

-- Yes, I know: it doesn't cover all the cases...

type GlobPattern = String

type Range = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob "" "" = True
matchGlob _ "" = False
matchGlob "" _ = False
matchGlob "*" _ = True
matchGlob ('?' : ps) (_ : ss) = matchGlob ps ss
matchGlob pat@('*' : ps) str@(s : ss) 
  | ps == str = True 
  | otherwise = matchGlob pat ss
matchGlob ('[' : ps) (h : t) = let (range, pRest) = matchRange "" ps in if h `elem` range then matchGlob pRest t else False
matchGlob x y = x == y

matchRange :: String -> GlobPattern -> (Range, GlobPattern)
matchRange acc "" = (acc, "")
matchRange acc (']' : ps) = (acc, ps)
matchRange acc (x : ps) = matchRange (x : acc) ps
