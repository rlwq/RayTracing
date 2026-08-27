module Main where

import Algebra
import Camera
import Data.Array
import Image
import Mesh
import Scene
import Shape

main :: IO ()
main = writePPM "out.ppm" (map (map shade) hits)
  where
    hits = map (map (`traceRay` scene)) (primaryRays (600, 400) camera)
    shade Missed = Vec3 0.04 0.05 0.08
    shade r = scale (0.25 + 0.75 * facing r) (irColor r)
    facing r = abs (dot (irNormal r) (Vec3 0 0 1))

writePPM :: FilePath -> [[Vec3]] -> IO ()
writePPM path = writeFile path . ppmText

camera :: Camera
camera = Camera (Vec3 0 0 0) 50 36 24

scene :: Scene
scene =
    Scene
        [ colored (Vec3 0.9 0.4 0.25) (meshShape (cube 22 (Vec3 0 0 (-170))))
        , colored (Vec3 0.3 0.55 0.95) (meshShape (cube 12 (Vec3 42 (-18) (-130))))
        , colored (Vec3 0.45 0.8 0.4) (meshShape (cube 9 (Vec3 (-38) 21 (-150))))
        ]

cube :: Scalar -> Point -> Mesh
cube size center =
    Mesh (listArray (0, 7) (map place corners)) (listArray (0, 11) sides)
  where
    corners =
        [ Vec3 x y z
        | x <- [-1, 1]
        , y <- [-1, 1]
        , z <- [-1, 1]
        ]
    place v = center + rotX 0.5 (rotY 0.6 (scale size v))
    sides =
        [ (0, 1, 3), (0, 3, 2)
        , (4, 6, 7), (4, 7, 5)
        , (0, 2, 6), (0, 6, 4)
        , (1, 5, 7), (1, 7, 3)
        , (2, 3, 7), (2, 7, 6)
        , (0, 4, 5), (0, 5, 1)
        ]

rotY :: Scalar -> Point -> Point
rotY a (Vec3 x y z) = Vec3 (cos a * x + sin a * z) y (cos a * z - sin a * x)

rotX :: Scalar -> Point -> Point
rotX a (Vec3 x y z) = Vec3 x (cos a * y - sin a * z) (sin a * y + cos a * z)
