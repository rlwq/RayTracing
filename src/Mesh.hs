module Mesh (
    Mesh (Mesh),
    VertexId,
    FaceId,
    Face,
    intersectMesh,
) where

import Algebra
import Data.Array
import Data.Semigroup (Arg (Arg), Min (Min))
import Geometry

type VertexId = Int

type FaceId = Int

type Vertex = Point

type Face = (VertexId, VertexId, VertexId)

data Mesh = Mesh (Array VertexId Vertex) (Array FaceId Face)

intersectMesh :: Ray -> Mesh -> Maybe (Distance, FaceId)
intersectMesh ray (Mesh vertices faces) =
    unwrap <$> foldMap mintersectFace (assocs faces)
  where
    triangle (a, b, c) = (vertices ! a, vertices ! b, vertices ! c)
    intersectFace face = intersectTriangle ray (triangle face)
    mintersectFace (i, face) =
        (\(t, _) -> Min (Arg t i)) <$> intersectFace face
    unwrap (Min (Arg t i)) = (t, i)
