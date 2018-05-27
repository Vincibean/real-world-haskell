-- Write a function splitWith that acts similarly to words but takes a predicate and a list of any type, 
-- and then splits its input list on every element for which the predicate returns False:

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = (els ++ h): (splitWith p t)
  where (els, rest) = span p xs
        (h, t) = if null rest 
                 then ([], []) 
                 else ([head rest], tail rest)
