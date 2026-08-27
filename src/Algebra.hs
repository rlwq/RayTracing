module Algebra (
    Scalar,
    Vec2 (..),
    Vec3 (..),
    Point,
    Direction,
    Normal,
    Distance,
    Vec (..),
    scale,
    quadrance,
    norm,
    normalize,
    cross,
) where

type Scalar = Float

data Vec2 = Vec2 !Scalar !Scalar
    deriving (Show, Eq)

data Vec3 = Vec3 !Scalar !Scalar !Scalar
    deriving (Show, Eq)

type Point = Vec3
type Direction = Vec3
type Normal = Vec3
type Distance = Scalar

class (Num v) => Vec v where
    splat :: Scalar -> v
    dot :: v -> v -> Scalar

scale :: (Vec v) => Scalar -> v -> v
scale s v = splat s * v

quadrance :: (Vec v) => v -> Scalar
quadrance v = dot v v

norm :: (Vec v) => v -> Scalar
norm = sqrt . quadrance

normalize :: (Vec v) => v -> v
normalize v = scale (1 / norm v) v

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 d e f) =
    Vec3 (b * f - c * e) (c * d - a * f) (a * e - b * d)

instance Num Vec2 where
    (+) :: Vec2 -> Vec2 -> Vec2
    (+) (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)
    (-) :: Vec2 -> Vec2 -> Vec2
    (-) (Vec2 a b) (Vec2 c d) = Vec2 (a - c) (b - d)
    (*) :: Vec2 -> Vec2 -> Vec2
    (*) (Vec2 a b) (Vec2 c d) = Vec2 (a * c) (b * d)
    abs :: Vec2 -> Vec2
    abs (Vec2 a b) = Vec2 (abs a) (abs b)
    signum :: Vec2 -> Vec2
    signum (Vec2 a b) = Vec2 (signum a) (signum b)
    negate :: Vec2 -> Vec2
    negate (Vec2 a b) = Vec2 (negate a) (negate b)
    fromInteger :: Integer -> Vec2
    fromInteger n = splat (fromInteger n)

instance Vec Vec2 where
    splat :: Scalar -> Vec2
    splat s = Vec2 s s
    dot :: Vec2 -> Vec2 -> Scalar
    dot (Vec2 a b) (Vec2 c d) = a * c + b * d

instance Num Vec3 where
    (+) :: Vec3 -> Vec3 -> Vec3
    (+) (Vec3 a b c) (Vec3 d e f) = Vec3 (a + d) (b + e) (c + f)
    (-) :: Vec3 -> Vec3 -> Vec3
    (-) (Vec3 a b c) (Vec3 d e f) = Vec3 (a - d) (b - e) (c - f)
    (*) :: Vec3 -> Vec3 -> Vec3
    (*) (Vec3 a b c) (Vec3 d e f) = Vec3 (a * d) (b * e) (c * f)
    abs :: Vec3 -> Vec3
    abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
    signum :: Vec3 -> Vec3
    signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)
    negate :: Vec3 -> Vec3
    negate (Vec3 a b c) = Vec3 (negate a) (negate b) (negate c)
    fromInteger :: Integer -> Vec3
    fromInteger n = splat (fromInteger n)

instance Vec Vec3 where
    splat :: Scalar -> Vec3
    splat s = Vec3 s s s
    dot :: Vec3 -> Vec3 -> Scalar
    dot (Vec3 a b c) (Vec3 d e f) = a * d + b * e + c * f
