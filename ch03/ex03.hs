-- Write a function that computes the number of elements in a list. 
-- To test it, ensure that it gives the same answers as the standard length function.

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs
