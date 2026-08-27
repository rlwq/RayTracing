module Scene (
    Scene (..),
    traceRay,
) where

import Ray
import Shape

newtype Scene = Scene {objects :: [Shape]}

traceRay :: Ray -> Scene -> IntersectResult
traceRay ray (Scene shapes) = foldMap (`intersectShape` ray) shapes
