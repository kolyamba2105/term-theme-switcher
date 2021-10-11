{-# LANGUAGE DeriveGeneric #-}

module Theme (Theme, colors) where

import           Data.Yaml
import           GHC.Generics

newtype Theme = Theme {colors :: Colors} deriving (Show, Generic)

instance ToJSON Theme

instance FromJSON Theme

-- Colors

data Colors = Colors
  { primary :: PrimaryColors,
    normal  :: RegularColors,
    bright  :: RegularColors,
    dim     :: Maybe RegularColors
  }
  deriving (Show, Generic)

instance ToJSON Colors

instance FromJSON Colors

-- PrimaryColors

data PrimaryColors = PrimaryColors
  { background :: String,
    foreground :: String
  }
  deriving (Show, Generic)

instance ToJSON PrimaryColors

instance FromJSON PrimaryColors

-- RegularColors

data RegularColors = RegularColors
  { black   :: String,
    blue    :: String,
    cyan    :: String,
    green   :: String,
    magenta :: String,
    red     :: String,
    white   :: String,
    yellow  :: String
  }
  deriving (Show, Generic)

instance ToJSON RegularColors

instance FromJSON RegularColors
