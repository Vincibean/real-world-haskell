-- Write the converse of fromList for the List type: a function that takes a List a and generates a [a].

data List a = Cons a (List a) | Nil deriving (Show)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

