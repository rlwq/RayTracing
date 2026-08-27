module Triangle (
    Triangle,
    intersectTriangle,
) where

import Algebra
import Control.Monad (guard)
import Ray

type Triangle = (,,) Point Point Point

intersectTriangle :: Ray -> Triangle -> Maybe (Distance, Point)
intersectTriangle (Ray o d) (v1, v2, v3) = do
    let e1 = v2 - v1
        e2 = v3 - v1
        pv = cross d e2
        det = dot e1 pv
    guard (abs det > epsilon)
    let invDet = 1 / det
        tv = o - v1
        u = dot tv pv * invDet
    guard (u >= 0 && u <= 1)
    let qv = cross tv e1
        v = dot d qv * invDet
    guard (v >= 0 && u + v <= 1)
    let t = dot e2 qv * invDet
    guard (t > epsilon)
    pure (t, o + scale t d)

epsilon :: Scalar
epsilon = 1e-6
