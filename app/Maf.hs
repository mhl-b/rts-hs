module Maf where

phi :: Float
phi = (1 + sqrt 5) / 2

phi2 :: Float
phi2 = phi * phi

phi3 :: Float
phi3 = phi2 * phi

floorf :: Float -> Float
floorf = fromInteger . floor

modf :: Float -> Float -> Float
modf x b = floorf (x / b) * b

