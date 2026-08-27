module Camera (
    Camera (..),
    primaryRay,
    primaryRays,
) where

import Algebra
import Geometry

data Camera = Camera
    { camOrigin :: Point
    , camFocal :: Scalar
    , camWidth :: Scalar
    , camHeight :: Scalar
    }
    deriving (Show, Eq)

primaryRay :: Camera -> (Scalar, Scalar) -> Ray
primaryRay (Camera o focal w h) (s, t) = Ray o (normalize d)
  where
    d = Vec3 (s * w / 2) (t * h / 2) (negate focal)

primaryRays :: (Int, Int) -> Camera -> [[Ray]]
primaryRays (nx, ny) cam =
    [ [primaryRay cam (ndcX i, ndcY j) | i <- [0 .. nx - 1]]
    | j <- [0 .. ny - 1]
    ]
  where
    ndcX i = 2 * (fromIntegral i + 0.5) / fromIntegral nx - 1
    ndcY j = 1 - 2 * (fromIntegral j + 0.5) / fromIntegral ny
