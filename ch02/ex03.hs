-- Write a function, lastButOne, that returns the element before the last.
lastButOne :: [a] -> a
lastButOne [x, _] = x
lastButOne (x: xs) = lastButOne xs

lastButOne' :: [a] -> a
lastButOne' xs = last (init xs)

lastButOne'' :: [a] -> a
lastButOne'' xs = last (take ((length xs) - 1) xs)


lastButOne''' :: [a] -> a
lastButOne''' = last . init
