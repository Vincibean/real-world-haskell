-- Define a function that joins a list of lists together using a separator value:

intersperse :: a -> [[a]] -> [a]
intersperse s xs = intersperseAcc [] xs
  where intersperseAcc acc [] = acc
        intersperseAcc acc (h:t) = intersperseAcc (acc ++ h ++ [s]) t
