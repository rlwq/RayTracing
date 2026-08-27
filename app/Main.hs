module Main where

import Algebra
import Camera
import Data.Array
import Image
import Mesh

main :: IO ()
main = writePPM "out.ppm" (map (map shade) hits)
  where
    hits = map (map (`intersectMesh` scene)) (primaryRays (600, 400) camera)
    depths = [t | row <- hits, Just (t, _) <- row]
    (near, far) = case depths of
        [] -> (0, 1)
        _ -> (minimum depths, maximum depths)
    span' = max 1e-6 (far - near)
    shade Nothing = Vec3 0.04 0.05 0.08
    shade (Just (t, _)) = splat (1 - (t - near) / span')

writePPM :: FilePath -> [[Vec3]] -> IO ()
writePPM path = writeFile path . ppmText

camera :: Camera
camera = Camera (Vec3 0 0 0) 50 36 24

scene :: Mesh
scene = Mesh (listArray (0, 7) (map place corners)) (listArray (0, 11) sides)
  where
    corners =
        [ Vec3 x y z
        | x <- [-1, 1]
        , y <- [-1, 1]
        , z <- [-1, 1]
        ]
    place = translate . rotX 0.5 . rotY 0.6 . scale 22
    translate v = v + Vec3 0 0 (-170)
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
