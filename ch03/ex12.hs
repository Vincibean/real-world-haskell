-- Consider three two-dimensional points, a, b, and c. 
-- If we look at the angle formed by the line segment from a to b and the line segment from b to c, it turns left, turns right, or forms a straight line. 
-- Define a Direction data type that lets you represent these possibilities.

data Direction = Left | Right | Straight deriving (Show, Eq)
