-- Small vec3 lib
-- see LICENSE
module Vec3 where

data Vec3 = Vec3 Double Double Double deriving (Eq, Show)

makeVec :: Double -> Vec3
makeVec t = Vec3 t t t

makeVec3 :: (Double, Double, Double) -> Vec3
makeVec3 (v1, v2, v3) = Vec3 v1 v2 v3

vlookup :: Int -> Vec3 -> Double
vlookup a (Vec3 x1 y1 z1)
    | a == 0 = x1
    | a == 1 = y1
    | a == 2 = z1
    | otherwise = error "Value error integer should not be bigger than 2"

vecx :: Vec3 -> Double
vecx v = vlookup 0 v

vecy :: Vec3 -> Double
vecy v = vlookup 1 v

vecz :: Vec3 -> Double
vecz v = vlookup 2 v

type VOp = Either Vec3 Double -- data Either a b - Left a - Right b

vneg :: Vec3 -> Vec3
vplus :: Vec3 -> VOp -> Vec3
vminus :: Vec3 -> VOp -> Vec3
vmultip :: Vec3 -> VOp -> Vec3
vdiv :: Vec3 -> VOp -> Vec3

vplus (Vec3 x y z) (Left (Vec3 a b c)) = Vec3 (x+a) (y+b) (c+z)
vplus (Vec3 x y z) (Right a)           = Vec3 (x+a) (y+a) (z+a)

vminus (Vec3 x y z) (Left (Vec3 a b c)) = Vec3 (x-a) (y-b) (c-z)
vminus (Vec3 x y z) (Right a)           = Vec3 (x-a) (y-a) (z-a)

vmultip (Vec3 x y z) (Left (Vec3 a b c)) = Vec3 (x*a) (y*b) (c*z)
vmultip (Vec3 x y z) (Right a)           = Vec3 (x*a) (y*a) (z*a)

vdiv (Vec3 x y z) (Left (Vec3 a b c))
    | a == 0 = error "first component is equal to 0"
    | b == 0 = error "second component is equal to 0"
    | c == 0 = error "third component is equal to 0"
    | otherwise = Vec3 (x/a) (y/b) (z/c)

vdiv (Vec3 x y z) (Right a)
    | a == 0 = error "division term is zero"
    | otherwise = Vec3 (x/a) (y/a) (z/a)

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
unit_vector v = v `vdiv` (Right (vlength v))

type Point3 = Vec3
type Color = Vec3
