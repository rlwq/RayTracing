module Image (
    ppmText,
) where

import Algebra

ppmText :: [[Vec3]] -> String
ppmText rows = header ++ body
  where
    nx = case rows of
        (r : _) -> length r
        [] -> 0
    ny = length rows
    header = unlines ["P3", show nx ++ " " ++ show ny, "255"]
    body = unlines (map (unwords . concatMap channels) rows)
    channels (Vec3 r g b) = map toByte [r, g, b]
    toByte x = show (round (255 * min 1 (max 0 x)) :: Int)
