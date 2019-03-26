-- The Prelude function concat concatenates a list of lists into a single list and has the following type:
--
--    concat :: [[a]] -> [a]
--
-- Write your own definition of concat using foldr

concat' :: [[a]] -> [a]
concat' = foldr (++) []
