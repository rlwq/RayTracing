module Mesh (
    Mesh (Mesh),
    Face (Face),
) where

import Algebra
import Data.Array

type Vertex = Point

data Face = Face Int Int Int
    deriving (Show, Eq)

data Mesh = Mesh (Array Int Vertex) [Face]
