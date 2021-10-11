module Error (Error(..)) where

import           Data.Yaml

data Error = InvalidUrl | InvalidFileName | FileParseError ParseException

instance Show Error where
  show InvalidUrl                 = "Invalid URL"
  show InvalidFileName            = "Invalid file name"
  show (FileParseError exception) = prettyPrintParseException exception
