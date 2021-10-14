module Lib (parseConfig) where

import           Config
import           Control.Monad
import           Data.ByteString           hiding (append)
import           Data.Either.Combinators
import qualified Data.HashMap.Strict       as SHM
import qualified Data.List                 as L
import           Data.Maybe
import qualified Data.String               as S
import qualified Data.Text                 as T
import           Data.Yaml
import           Error
import           Fetch
import           Filesystem.Path.CurrentOS
import           ListUtils
import           Network.URI
import           Options.Generic
import           Prelude                   hiding (FilePath)
import           System.Directory
import           Theme

parseConfig :: IO ()
parseConfig = do
  config <- getRecord $ T.pack "Alacritty theme swither"
  home <- getHomeDirectory
  case config of
    Local configPath themePath -> applyTheme (toConfigPath home (unHelpful configPath)) (decodeLocalTheme $ toPath themePath)
    Remote configPath themeUrl -> applyTheme (toConfigPath home (unHelpful configPath)) (decodeRemoteTheme $ unHelpful themeUrl)
  where
    toPath = fromText . T.pack . unHelpful

toConfigPath :: String -> Maybe String -> FilePath
toConfigPath home = maybe (fromString home </> fromString ".config/alacritty/alacritty.yml") fromString
  where
    fromString = fromText . T.pack

applyTheme :: FilePath -> IO (Either Error Theme) -> IO ()
applyTheme configPath applyTheme = do
  config  <- decodeConfig configPath
  theme   <- applyTheme
  either print id $ liftM2 (saveTheme configPath) config theme

saveTheme :: FilePath -> Object -> Theme -> IO ()
saveTheme name config theme = encodeFile (encodeString name) (replaceTheme theme config)

replaceTheme :: Theme -> Object -> SHM.HashMap T.Text Value
replaceTheme theme = SHM.insert colorsTag (toJSON $ colors theme) . SHM.map toJSON . SHM.filterWithKey (\key _ -> key /= colorsTag)
  where
    colorsTag = T.pack "colors"

decodeConfig :: FilePath -> IO (Either Error Object)
decodeConfig = fmap (mapLeft FileParseError) . decodeFileEither . encodeString

decodeLocalTheme :: FilePath -> IO (Either Error Theme)
decodeLocalTheme = fmap (mapLeft FileParseError) . decodeFileEither . encodeString

decodeRemoteTheme :: String -> IO (Either Error Theme)
decodeRemoteTheme url =
  case uriFromString url of
    Left error -> pure $ Left error
    Right url  -> mapLeft FileParseError . decodeEither' <$> fetch url
