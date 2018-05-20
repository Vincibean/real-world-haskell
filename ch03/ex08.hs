import Data.List
-- Create a function that sorts a list of lists based on the length of each sublist. 
-- (You may want to look at the sortBy function from the Data.List module.)

sortBySublistLength :: [[a]] -> [[a]]
sortBySublistLength = sortBy compareByLength
  where compareByLength a b = compare (length a) (length b)
