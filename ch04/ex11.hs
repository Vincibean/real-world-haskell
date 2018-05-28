import Data.Char (isSpace)

-- How many of the following Prelude functions can you rewrite using list folds?
--  • any
--  • cycle
--  • words
--  • unlines
-- For those functions where you can use either foldl' or foldr, which is more appropriate in each case?

any' :: Foldable t => (a -> Bool) -> t a -> Bool
any' p = foldr step False
  where step el acc
          | p el = True
          | otherwise = acc


-- here the fold is admittedly useless
cycle' :: [a] -> [a]
cycle' xs = foldr (:) [] myL
  where myL = concat (loop xs)
        loop els = els : loop els

words' :: String -> [String]
words' ss = filter (not . null) partial
  where partial = foldr step [""] ss
        step el acc@(h:t)
          | isSpace el = "" : acc
          | otherwise = (el : h) : t

unlines' :: [String] -> String
unlines' = foldr step ""
  where step el acc = el ++ "\n" ++ acc
