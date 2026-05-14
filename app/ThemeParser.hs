module ThemeParser where

import Toml (decode, Result(..))
import Toml.Schema.FromValue (FromValue(..), parseTableFromValue, reqKey)
import System.IO (hPutStrLn, stderr )
import qualified Data.Text  as T (Text, pack) 
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

data ThemeConfig = ThemeConfig {
    themeDetail :: ThemeSection,
    colorDetail :: ColorSection
} deriving(Show)

newtype ThemeSection = ThemeSection {
    name :: T.Text 
} deriving (Show)

newtype ColorSection = ColorSection {
    colors :: [[Int]]
} deriving (Show)


instance FromValue ThemeConfig where
    fromValue = parseTableFromValue $ ThemeConfig
        <$> reqKey (T.pack "theme")
        <*> reqKey (T.pack "color")

instance FromValue ThemeSection where
    fromValue = parseTableFromValue $ ThemeSection
        <$> reqKey (T.pack "name")


instance FromValue ColorSection where
    fromValue = parseTableFromValue $ ColorSection
        <$> reqKey (T.pack "color")



parseToml :: FilePath -> IO ThemeConfig
parseToml filepath = do
    content <- TIO.readFile filepath 

    case decode content of 
        Failure errs -> do
            hPutStrLn stderr "Failed to read valid toml configuration"
            hPutStrLn stderr (unlines errs)
            exitFailure
        Success _ config -> do
            return config
