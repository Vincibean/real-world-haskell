import Geometry
-- Define a function that takes a list of two-dimensional points and computes the direction of each successive triple. 
-- Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c], then the turn made by [b,c,d], then [c,d,e]. 
-- Your function should return a list of Direction.

turns :: [Point2D] -> [Direction]
turns (p1 : p2 : p3 : ps) = turn p1 p2 p3 : turns (p2:p3:ps)
turns _ = []
