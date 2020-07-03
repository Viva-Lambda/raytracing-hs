-- Small vec3 lib
-- see LICENSE
-- author: Kaan ERASLAN
module Vec3 where

data Vec3 = Vec3 Double Double Double deriving (Eq, Show)

lookup :: Int -> Vec3 -> Double
lookup a (Vec3 x y z)
    | a == 0 = x
    | a == 1 = y
    | a == 2 = z
    | otherwise = error "Value error integer should not be bigger than 2"

data VOp = Either Vec3 Double

vneg :: Vec3 -> Vec3
vplus :: Vec3 -> Vec3 -> Vec3
vminus :: Vec3 -> Vec3 -> Vec3
vmultip :: Vec3 -> Vec3 -> Vec3
vdiv :: Vec3 -> Vec3 -> Vec3

vplus (Vec3 x y z) (Vec3 a b c) = Vec3 (x+a) (y+b) (c+z)
vminus (Vec3 x y z) (Vec3 a b c) = Vec3 (x-a) (y-b) (c-z)
vmultip (Vec3 x y z) (Vec3 a b c) = Vec3 (x*a) (y*b) (c*z)
vdiv (Vec3 x y z) (Vec3 a b c) = Vec3 (x/a) (y/b) (c/z)
vneg (Vec3 x y z) = Vec3 (-x) (-y) (-z)

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x y z) (Vec3 a b c) = (x*a) + (y*b)+(z*c)

length_squared :: Vec3 -> Double
length_squared v = dot v v

vlength :: Vec3 -> Double
vlength v = (length_squared v) ** (1/2::Double)

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 p1x p1y p1z) (Vec3 p2x p2y p2z) = Vec3 (p1y * p2z - p1z * p2y) (p1z * p2x - p1x * p2z) (p1x * p2y - p1y * p2x)

unit_vector :: Vec3 -> Vec3
unit_vector v = v `vdiv` (vlength v)

type Point3 = Vec3
type Color = Vec3
