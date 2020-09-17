module Geometry.Cuboid
  ( volume,
    area,
  )
where

volume a b c = rectArea a b * c

area a b c = rectArea a b * 2 + rectArea b c * 2 + rectArea c a * 2

rectArea a b = a * b