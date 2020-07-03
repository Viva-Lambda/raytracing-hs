module Main where

--import Control.Monad

image_width :: Int
image_width = 256
image_height :: Int
image_height = 256

doNothing :: IO()
doNothing = return()

pixDiv :: Int -> Int -> Double
pixDiv i imw = (fromIntegral i) / (fromIntegral (imw - 1))
pixMul :: Double -> Double -> Int
pixMul a b = floor (a * b)


printColor :: Int -> Int -> IO()
printColor i j  =
    let iterm = pixDiv i image_width
        jterm = pixDiv j image_height
        r = pixMul 255.99 iterm
        g = pixMul 255.99 jterm
        b = pixMul 255.99 0.25
    in putStrLn ((show r) ++ " " ++ (show g) ++ " " ++ (show b))

zipMerge :: [a] -> [b] -> [(a,b)]
zipMerge [] _      = []
zipMerge _  []     = error "second list cannot be empty"
zipMerge (a:as) bs = (zip (replicate (length bs) a) bs) ++ (zipMerge as bs)

-- foldl :: (a -> b -> a) -> a -> [b] -> a
printPixel :: IO() -> (Int, Int) -> IO()
printPixel _ (i, j) = printColor i j

printPixels :: [(Int, Int)] -> IO()
printPixels colors = foldl printPixel doNothing colors

printGradient :: IO()
printGradient = do
    putStrLn "P3"
    putStrLn ((show image_width) ++ " " ++ (show image_height))
    putStrLn "255"
    let is = [0..256]
        js = [256,255..0]
    printPixels (zipMerge js is)


main :: IO ()
main = printGradient
