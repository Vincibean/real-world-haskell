module Geometry where

data Point2D = Point2D { x, y :: Double } deriving (Show, Ord, Eq)

data Direction = Left | Right | Straight deriving (Show, Eq)

turn :: Point2D -> Point2D -> Point2D -> Direction
turn p1 p2 p3
    | clockwise > 0 = Geometry.Left
    | clockwise < 0 = Geometry.Right
    | otherwise = Straight
        where clockwise = (x p2 - x p1) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)

turns :: [Point2D] -> [Direction]
turns (p1 : p2 : p3 : ps) = turn p1 p2 p3 : turns (p2:p3:ps)
turns _ = []
