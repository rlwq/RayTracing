module Shape (
    IntersectResult (..),
    Shape (..),
    colored,
) where

import Algebra
import Ray

data IntersectResult
    = Missed
    | Hit
        { irDistance :: Distance
        , irPoint :: Point
        , irNormal :: Normal
        , irColor :: Vec3
        }
    deriving (Show, Eq)

instance Semigroup IntersectResult where
    (<>) :: IntersectResult -> IntersectResult -> IntersectResult
    (<>) Missed b = b
    (<>) a Missed = a
    (<>) a b
        | irDistance a <= irDistance b = a
        | otherwise = b

instance Monoid IntersectResult where
    mempty :: IntersectResult
    mempty = Missed

newtype Shape = Shape {intersectShape :: Ray -> IntersectResult}

instance Semigroup Shape where
    (<>) :: Shape -> Shape -> Shape
    (<>) (Shape f) (Shape g) = Shape (\ray -> f ray <> g ray)

instance Monoid Shape where
    mempty :: Shape
    mempty = Shape (const Missed)

colored :: Vec3 -> Shape -> Shape
colored c (Shape f) = Shape (paint . f)
  where
    paint Missed = Missed
    paint h = h{irColor = c}
