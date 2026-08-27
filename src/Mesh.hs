module Mesh (
    Mesh (Mesh),
    VertexId,
    FaceId,
    Face,
    meshShape,
) where

import Algebra
import Data.Array
import Shape
import Triangle

type VertexId = Int

type FaceId = Int

type Vertex = Point

type Face = (VertexId, VertexId, VertexId)

data Mesh = Mesh (Array VertexId Vertex) (Array FaceId Face)

meshShape :: Mesh -> Shape
meshShape (Mesh vertices faces) =
    Shape (\ray -> foldMap (intersectFace ray) (elems faces))
  where
    intersectFace ray (i, j, k) = case intersectTriangle ray (a, b, c) of
        Nothing -> Missed
        Just (t, p) -> Hit t p (normalize (cross (b - a) (c - a))) (splat 1)
      where
        a = vertices ! i
        b = vertices ! j
        c = vertices ! k
