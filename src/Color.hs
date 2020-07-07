-- license, see LICENSE
module Color(printColor, pixDiv) where

import Vec3

pixDiv :: Int -> Int -> Double
pixDiv a b = (fromIntegral a) / (fromIntegral b)

pixMul :: Double -> Double -> Int
pixMul a b = floor (a * b)

printColor :: Color -> IO()

printColor (Vec3 x y z) = do
            let r     = pixMul 255.999 x
                g     = pixMul 255.999 y
                b     = pixMul 255.999 z
            putStrLn ((show r) ++ " " ++ (show g) ++ " " ++ (show b))
