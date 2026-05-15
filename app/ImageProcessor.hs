module ImageProcessor where

import Codec.Picture 
import qualified Data.Vector as V
import System.IO (hPutStrLn, stderr )
import System.Exit (exitFailure)

data ImageDetail = ImageDetail {
    imageList :: [[Int]],
    width :: Int,
    height :: Int
}


getEuclideanDistance :: [Int] -> [Int] -> Int 
getEuclideanDistance xs ys = round $ sqrt $ sum $ zipWith (\x y -> (fromIntegral x - fromIntegral y) ^ 2) xs ys

loadImage :: FilePath -> IO ImageDetail 
loadImage filepath = do
    loaded <- readImage filepath
    case loaded of
        Left err -> do
            hPutStrLn stderr "Failed to load Image"
            exitFailure 
        Right dynamicImage -> do
            let img = convertRGB8 dynamicImage
                w = imageWidth img
                h = imageHeight img

                imgList  = [ [fromIntegral r, fromIntegral g, fromIntegral b] | y <- [0 .. h-1],x <- [0 .. w-1],  let PixelRGB8 r g b = pixelAt img x y]
            return ImageDetail {imageList = imgList, width = w ,height = h}


applyTheme :: [[Int]] -> [[Int]] -> [[Int]]
applyTheme image theme = map findClosestColor image
    where
        findClosestColor pixel = 
            let distances = [ (getEuclideanDistance pixel themeColor, themeColor) | themeColor <- theme]
            in snd (minimumDistance distances)

        minimumDistance = foldl1 (\acc x -> if fst x < fst acc then x else acc) 



generateOutput :: FilePath -> [[Int]] -> Int -> Int -> IO ()
generateOutput outPath pixelList w h = do
    let pixelVector = V.fromList pixelList 
    let
        getPixel x y = 
            case pixelVector V.! (y * w + x) of
                (r:g:b:_) -> PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
                _         -> PixelRGB8 0 0 0 

    let finalImage = generateImage getPixel w h 
    writePng outPath finalImage

