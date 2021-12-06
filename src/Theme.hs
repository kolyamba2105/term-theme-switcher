{-# LANGUAGE DeriveGeneric #-}

module Theme
  ( Theme
  , replace
  , reset
  ) where

import qualified Data.HashMap.Strict as SHM
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics

newtype Theme =
  Theme
    { colors :: Colors
    }
  deriving (Show, Generic)

instance ToJSON Theme

instance FromJSON Theme

data Colors =
  Colors
    { primary :: PrimaryColors
    , normal :: RegularColors
    , bright :: RegularColors
    , dim :: Maybe RegularColors
    }
  deriving (Show, Generic)

instance ToJSON Colors

instance FromJSON Colors

data PrimaryColors =
  PrimaryColors
    { background :: String
    , foreground :: String
    }
  deriving (Show, Generic)

instance ToJSON PrimaryColors

instance FromJSON PrimaryColors

data RegularColors =
  RegularColors
    { black :: String
    , blue :: String
    , cyan :: String
    , green :: String
    , magenta :: String
    , red :: String
    , white :: String
    , yellow :: String
    }
  deriving (Show, Generic)

instance ToJSON RegularColors

instance FromJSON RegularColors

replace :: Theme -> Object -> SHM.HashMap T.Text Value
replace theme = SHM.insert colorsTag (toJSON $ colors theme) . reset

reset :: Object -> SHM.HashMap T.Text Value
reset = SHM.map toJSON . SHM.filterWithKey (\key _ -> key /= colorsTag)

colorsTag :: T.Text
colorsTag = T.pack "colors"
