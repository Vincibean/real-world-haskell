import Data.Char (digitToInt)

-- Use a fold (choosing the appropriate fold will make your code much simpler) to rewrite and improve upon the asInt function 
-- from the earlier section“Explicit Recursion” on page 85.
-- Your function should behave as follows:	
-- 	
--   ghci> asInt_fold "101"	
--   101	
--   ghci> asInt_fold "-31337"	
--   -31337	
--   ghci> asInt_fold "1798"	
--   1798

asInt_fold :: String -> Int
asInt_fold str@(sign:num) 
    | sign == '-' = -1 * loop num
    | otherwise = loop str
  where loop xs = foldl (\i c -> i * 10 + digitToInt c) 0 xs
