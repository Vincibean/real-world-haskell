--  Write your own definition of the standard takeWhile function, first using explicit recursion, and then foldr.

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (h:t)
  | p h = h : takeWhile' p t
  | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p xs = reverse (loop p [] xs)
  where loop _ acc [] = acc
        loop p acc (h:t)
          | p h = loop p (h : acc) t
          | otherwise = acc

takeWhile''' :: (a -> Bool) -> [a] -> [a]
takeWhile''' p xs = foldr step [] xs
  where step el acc
          | p el = el : acc
          | otherwise = []
