module Fetch
  ( fetch
  , uriFromString
  ) where

import Data.ByteString
import Data.ByteString.Lazy hiding (ByteString)
import Data.Either.Combinators
import Error
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.URI

networkManager :: IO Manager
networkManager = newManager tlsManagerSettings

fetch :: URI -> IO ByteString
fetch url = do
  manager <- networkManager
  request <- requestFromURI url
  toStrict . responseBody <$> httpLbs request manager

uriFromString :: String -> Either Error URI
uriFromString = maybeToRight InvalidUrl . parseURI
