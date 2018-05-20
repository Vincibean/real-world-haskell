module Geometry where

data Point2D = Point2D { x:: Double, y :: Double } deriving (Show, Ord, Eq)

data Direction = Left | Right | Straight deriving (Show, Eq)
