module Lib
  ( parseConfig
  ) where

import Config
import Control.Monad
import Data.Either.Combinators
import qualified Data.Text as T
import Data.Yaml
import Error
import Fetch
import Filesystem.Path.CurrentOS
import Options.Generic
import Prelude hiding (FilePath)
import System.Directory
import Theme

parseConfig :: IO ()
parseConfig = do
  config <- getRecord $ T.pack "Alacritty theme swither"
  home <- getHomeDirectory
  let fromString = fromText . T.pack
      makePath = fromString . unHelpful
      makeConfigPath =
        maybe (fromString home </> fromString ".config/alacritty/alacritty.yml") fromString .
        unHelpful
  case config of
    Local configPath themePath ->
      applyTheme (makeConfigPath configPath) (decodeFile' $ makePath themePath)
    Remote configPath themeUrl ->
      applyTheme (makeConfigPath configPath) (decodeRemoteTheme $ unHelpful themeUrl)
    Reset configPath -> resetTheme $ makeConfigPath configPath

applyTheme :: FilePath -> IO (Either Error Theme) -> IO ()
applyTheme configPath applyTheme = do
  config <- decodeFile' configPath
  theme <- applyTheme
  either print id $ liftM2 (saveTheme configPath) config theme
  where
    saveTheme name config theme = encodeFile (encodeString name) (replace theme config)

decodeFile' :: FromJSON a => FilePath -> IO (Either Error a)
decodeFile' = fmap (mapLeft FileParseError) . decodeFileEither . encodeString

decodeRemoteTheme :: String -> IO (Either Error Theme)
decodeRemoteTheme url =
  case uriFromString url of
    Left error -> pure $ Left error
    Right url -> mapLeft FileParseError . decodeEither' <$> fetch url

resetTheme :: FilePath -> IO ()
resetTheme configPath = do
  config <- decodeFile' configPath
  either print saveTheme config
  where
    saveTheme = encodeFile (encodeString configPath) . reset
