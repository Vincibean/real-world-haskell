import Geometry
import Data.List
import Data.Ord (comparing)

-- Using the code from the preceding three exercises, implement Graham’s scan algorithm for the convex hull of a set of 2D points. 
-- You can find good description of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull) is, 
-- and how the Graham scan algorithm (http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia (http://en.wikipedia.org/).
--
-- Slight modification of bendoerr's solution (https://github.com/bendoerr/real-world-haskell/blob/master/ch03/Exercises.hs)

sortByY xs = sortBy lowestY xs
             where lowestY a b = compare (y a, x a) (y b, x b)

pointAngle a b = (x b - x a) / (y b - y a)
 
pointOrdering a b = compare (pointAngle a b) 0.0
 
sortByAngle ps = bottomLeft : sortBy (compareAngles bottomLeft) (tail (sortedPs))
                where sortedPs = sortByY ps
                      bottomLeft = head (sortedPs)
  
compareAngles = comparing . pointAngle

grahamScan ps = scan (sortByAngle ps)
           where scan (a:v:b:xs) = if turn a v b == Geometry.Right
                                   then a : scan(b:xs)
                                   else a : scan(v:b:xs)
                 scan [a, b]      = [a, b]
                 scan _          = []
