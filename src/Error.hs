module Error
  ( Error(..)
  ) where

import Data.Yaml

data Error
  = InvalidUrl
  | InvalidConfigPath
  | FileParseError ParseException

instance Show Error where
  show InvalidUrl = "Invalid URL!"
  show InvalidConfigPath = "Invalid config path!"
  show (FileParseError exception) = prettyPrintParseException exception
