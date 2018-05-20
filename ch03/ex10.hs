-- The separator should appear between elements of the list, but it should not follow the last element. 
-- Your function should behave as follows:
-- 
--   ghci> :load Intersperse 
--   [1 of 1] Compiling Main        ( Intersperse.hs, interpreted )
--   Ok, modules loaded: Main. 
--   ghci> intersperse ',' [] 
--   ""
--   ghci> intersperse ',' ["foo"]
--   "foo"
--   ghci> intersperse ',' ["foo","bar","baz","quux"] 
--   "foo,bar,baz,quux"

intersperse :: a -> [[a]] -> [a]
intersperse s xs = intersperseAcc [] xs
  where intersperseAcc acc [] = acc
        intersperseAcc [] (h:t) = intersperseAcc h t
        intersperseAcc acc (h:t) = intersperseAcc (acc ++ [s] ++ h) t
