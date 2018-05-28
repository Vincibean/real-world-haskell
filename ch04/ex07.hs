import Data.Char (digitToInt, isDigit)  

-- The asInt_fold function uses error, so its callers cannot handle errors. Rewrite the function to fix this problem:
--
--    ghci> asInt_either "33"
--    Right 33
--    ghci> asInt_either "foo"
--    Left "non-digit 'o'"

type ErrorMessage = String

asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "Empty String" 
asInt_either str@(sign:num)
    | null num && not (isDigit sign) = Left "Invalid Character"
    | notAllDigits num = Left "We handle Integers only"
    | tooLarge str = Left "too large a number"
    | sign == '-' = Right (-1 * loop num)
    | otherwise = Right (loop str)
  where loop = foldl (\i c -> i * 10 + digitToInt c) 0
        notAllDigits xs = not (all isDigit xs)
        tooLarge s = (length s) >= (length (show (maxBound :: Int)))
