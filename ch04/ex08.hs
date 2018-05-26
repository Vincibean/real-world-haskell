-- The asInt_fold function uses error, so its callers cannot handle errors. Rewrite the function to fix this problem:
--
--    ghci> asInt_either "33"
--    Right 33
--    ghci> asInt_either "foo"
--    Left "non-digit 'o'"

type ErrorMessage = String

asInt_either :: String -> Ei
asInt_either = undefined
