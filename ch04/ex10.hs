-- The Data.List module defines a function, groupBy, which has the following type:
--
--    groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
--
-- Use ghci to load the Data.List module and figure out what groupBy does, then write your own implementation using a fold.

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f = foldr step [[]]
  where step el ([]:t) = [el] : t
        step el acc
          | f el (head (head acc)) = (el : head acc) : (tail acc)
          | otherwise = [el] : acc
