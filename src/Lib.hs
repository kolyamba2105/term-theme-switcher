module Lib (parseConfig) where

import           Config
import           Control.Monad
import           Data.ByteString
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
import           Theme

decodeConfig :: FilePath -> IO (Either Error Object)
decodeConfig = fmap (mapLeft FileParseError) . decodeFileEither . encodeString

decodeLocalTheme :: FilePath -> IO (Either Error Theme)
decodeLocalTheme = fmap (mapLeft FileParseError) . decodeFileEither . encodeString

fetchTheme :: String -> IO (Either Error Theme)
fetchTheme url =
  case uriFromString url of
    Left error -> pure $ Left error
    Right url  -> mapLeft FileParseError . decodeEither' <$> fetch url

applyRemoteTheme :: FilePath -> String -> IO ()
applyRemoteTheme configPath themeUrl = do
  config  <- decodeConfig configPath
  theme   <- fetchTheme themeUrl
  either print id $ liftM3 saveTheme name config theme
    where
      name = maybeToRight InvalidFileName (fromText . T.pack <$> (parseURI themeUrl >>= extractThemeName))
      extractThemeName = fmap T.unpack . ListUtils.last . T.splitOn (T.pack "/") . T.pack . uriPath

applyLocalTheme :: FilePath -> FilePath -> IO ()
applyLocalTheme configPath themePath = do
  config  <- decodeConfig configPath
  theme   <- decodeLocalTheme themePath
  either print id $ liftM2 (saveTheme $ filename themePath) config theme

saveTheme :: FilePath -> Object -> Theme -> IO ()
saveTheme name config theme = encodeFile (encodeString name) (applyTheme theme config)

applyTheme :: Theme -> Object -> SHM.HashMap T.Text Value
applyTheme theme = SHM.insert colorsTag (toJSON $ colors theme) . SHM.map toJSON . SHM.filterWithKey (\key _ -> key /= colorsTag)
  where
    colorsTag = T.pack "colors"

parseConfig :: IO ()
parseConfig = do
  config <- getRecord $ T.pack "Alacritty theme swither"
  case config of
    Local configPath themePath -> applyLocalTheme (toPath configPath) (toPath themePath)
    Remote configPath themeUrl -> applyRemoteTheme (toPath configPath) (unHelpful themeUrl)
  where
    toPath = fromText . T.pack . unHelpful
