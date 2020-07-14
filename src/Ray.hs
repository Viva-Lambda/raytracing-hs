-- Ray object

module Ray where

import Vec3

data Ray = Ray Point3 Vec3 deriving (Eq, Show)

origin :: Ray -> Point3

origin (Ray orig dir) = orig

direction :: Ray -> Vec3
direction (Ray orig dir) = dir

at :: Ray -> Double -> Point3
at (Ray orig dir) t = vplus orig (Left (vmultip dir (Right t)))
