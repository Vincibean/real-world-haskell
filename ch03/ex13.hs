import Geometry
-- Write a function that calculates the turn made by three two-dimensional points and returns a Direction.

turn :: Point2D -> Point2D -> Point2D -> Direction
turn p1 p2 p3
    | clockwise > 0 = Geometry.Right
    | clockwise < 0 = Geometry.Left
    | otherwise = Straight
  where clockwise = (x p2 - x p1) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)
