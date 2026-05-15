{-# LANGUAGE OverloadedStrings #-}
module Main where

import ThemeParser
import ImageProcessor
import Options.Applicative
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure )

-- application settings 
data CMap = CMap {
    image :: FilePath,
    theme :: FilePath,
    output :: FilePath
}

cmap :: Parser CMap
cmap = CMap
    <$> strOption
        (long "image"
        <> short 'i'
        <> metavar "IMAGEPATH"
        <> help "provide image to execute on")
    <*> strOption 
        (long "theme"
        <> short 't'
        <> metavar "THEMEPATH"
        <> help "path to theme")
    <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "OUTPUTPATH"
        <> help "provide output path")


main :: IO ()
main = parse =<< execParser opts
    where
        opts = info (cmap <**> helper)
            ( fullDesc 
            <> progDesc "Convert an image to a specific theme"
            <> header "cmap - CLI image theming utility" )

parse :: CMap -> IO ()
parse (CMap imagePath themePath outputPath) = do
    if takeExtension themePath /= ".toml"
        then do
            hPutStrLn stderr "Error: Theme file must have a '.toml' extension."
            exitFailure
        else do
            config <- parseToml themePath
            let themeName = name (themeDetail config)
                themeColors = colors (colorDetail config)
            putStrLn $ "Processing image: " ++ imagePath
            putStrLn $ "Applying theme " ++ show themeName 
            loadedImageData <- loadImage imagePath 
            let loadedImage = imageList loadedImageData
                w = width loadedImageData
                h = height loadedImageData
            let outputImage = applyTheme loadedImage themeColors 
            generateOutput outputPath outputImage w h
            putStrLn $ "Output image generated at " ++ outputPath

            
            
            
            

