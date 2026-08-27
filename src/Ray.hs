module Ray (
    Ray (..),
) where

import Algebra

data Ray = Ray Point Direction
    deriving (Show, Eq)
