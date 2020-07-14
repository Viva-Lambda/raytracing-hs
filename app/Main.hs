module Main where

import Color ( pixDiv, printColor )
import Ray
import Vec3

hitSphere :: Vec3 -> Double -> Ray -> Bool

hitSphere p radius (Ray rorig rdir) = let oc = rorig `vminus` (Left p)
                                          a = dot rdir rdir
                                          b = 2.0 * (dot oc rdir)
                                          c = (dot oc oc) - (radius * radius)
                                          d = (b*b) - (4*a*c)
                                      in d > 0

rayOrColor :: Bool -> Vec3 -> VOp -> Vec3
rayOrColor b v1 v2
    | b == True = Vec3 0.5 0.5 0
    | otherwise = v1 `vplus` v2


ray_color :: Ray -> Color
ray_color r = let unit = unit_vector (direction r)
                  t = 0.5 * ((vecy unit) + 1)
                  alpha = Right (1 - t)
                  sorig = makeVec3 (0.0, 0.0, -1.0)
                  radius = 0.5
                  isHit = hitSphere sorig radius r
                  first = vmultip (makeVec 1.0) (Right t)
                  second = Left (vmultip (Vec3 0.5 0.7 1.0) alpha)
                  in rayOrColor isHit first second


aspect_ratio :: Double
aspect_ratio = 16.0 / 9.0

image_width :: Int
image_width = 354

image_height :: Int
image_height = floor ((fromIntegral image_width) / aspect_ratio)

viewport_height :: Double
viewport_height = 2.0

viewport_width :: Double
viewport_width = aspect_ratio * viewport_height

focal_length :: Double
focal_length = 1.0
camera_origin :: Point3
camera_origin = makeVec 0.0
camera_horizontal = Vec3 viewport_width 0 0
camera_vertical = Vec3 viewport_height 0 0
camera_lower_left :: Vec3
camera_lower_left = let hhalf = Left (vdiv camera_horizontal (Right 2.0))
                        vhalf = Left (vdiv camera_vertical (Right 2.0))
                        origin_hdiff = camera_origin `vminus` hhalf
                        origin_vdiff = origin_hdiff `vminus` vhalf
                    in origin_vdiff `vminus` (Left (Vec3 0.0 0.0 focal_length))

zipMerge :: [a] -> [b] -> [(a, b)]
zipMerge []       _  = []
zipMerge _        [] = error "zipMerge second list cannot be empty"
zipMerge (a : as) bs = (zip (replicate (length bs) a) bs) ++ (zipMerge as bs)

makeColor :: (Int, Int) -> Color
makeColor (i, j) = let u = Right ( pixDiv i (image_width - 1))
                       v = Right (pixDiv j (image_height - 1))
                       uh = Left (camera_horizontal `vmultip`  u)
                       vv = Left (camera_vertical `vmultip`  v)
                       dir = camera_lower_left `vplus` uh `vplus` vv `vminus` (Left camera_origin)
                       r = Ray camera_origin dir
                    in ray_color(r)

-- foldl :: (a -> b -> a) -> a -> [b] -> a
printPixel :: (Int, Int) -> IO ()
printPixel pixCoord = printColor (makeColor pixCoord)

printPixels :: [(Int, Int)] -> IO ()
printPixels colors = mapM_ printPixel colors

printGradient :: IO ()
printGradient = do
    putStrLn "P3"
    putStrLn ((show image_width) ++ " " ++ (show image_height))
    putStrLn "255"
    let is = [0 .. image_width]
        js = image_height:[254, 253 .. 0]
    printPixels (zipMerge is js)


main :: IO ()
main = printGradient
