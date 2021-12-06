{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Config
  ( Config(..)
  ) where

import Options.Generic

data Config
  = Local
      { config :: Maybe String <?> "Path to initial config file"
      , path :: String <?> "Path to theme file"
      }
  | Remote
      { config :: Maybe String <?> "Path to initial config file"
      , url :: String <?> "URL to theme file"
      }
  | Reset
      { config :: Maybe String <?> "Path to initial config file"
      }
  deriving (Generic, Show)

instance ParseRecord Config where
  parseRecord = parseRecordWithModifiers $ lispCaseModifiers {shortNameModifier = firstLetter}
