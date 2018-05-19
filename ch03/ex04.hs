-- Add a type signature for your function to your source file. To test it,load the source file into ghci again.

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs
