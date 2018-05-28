import Data.Char (digitToInt, isDigit)

-- Extend your function to handle the following kinds of exceptional conditions by calling error:
--
--    ghci> asInt_fold ""
--    0
--    ghci> asInt_fold "-"
--    0
--    ghci> asInt_fold "-3"
--    -3
--    ghci> asInt_fold "2.7"
--    *** Exception: Char.digitToInt: not a digit '.'
--    ghci> asInt_fold "314159265358979323846"
--    564616105916946374

asInt_fold :: String -> Int
asInt_fold "" = error "Empty String" 
asInt_fold str@(sign:num)
    | null num && not (isDigit sign) = error "Invalid Character"
    | notAllDigits num = error "We handle Integers only"
    | tooLarge str = error "too large a number"
    | sign == '-' = -1 * loop num
    | otherwise = loop str
  where loop = foldl (\i c -> i * 10 + digitToInt c) 0
        notAllDigits xs = not (all isDigit xs)
        tooLarge s = (length s) >= (length (show (maxBound :: Int)))
