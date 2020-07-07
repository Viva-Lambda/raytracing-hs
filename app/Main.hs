module Main where

import Color ( printColor, pixDiv )
import Vec3

image_width :: Int
image_width = 256
image_height :: Int
image_height = 256

zipMerge :: [a] -> [b] -> [(a, b)]
zipMerge []       _  = []
zipMerge _        [] = error "second list cannot be empty"
zipMerge (a : as) bs = (zip (replicate (length bs) a) bs) ++ (zipMerge as bs)

makeColor :: (Int, Int) -> Color
makeColor (i, j) = Vec3 (pixDiv i (image_width - 1)) (pixDiv j (image_height - 1)) 0.25

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
    let is = [0 .. 255]
        js = [255, 254 .. 0]
    printPixels (zipMerge is js)


main :: IO ()
main = printGradient
