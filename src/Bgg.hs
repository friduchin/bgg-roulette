module Bgg where
-- bgg API wrapper

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BC

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host method path = setRequestMethod method
                                $ setRequestHost host
                                $ setRequestPath path
                                $ setRequestSecure True
                                $ setRequestPort 443
                                $ defaultRequest


hostName :: BC.ByteString
hostName = "www.boardgamegeek.com"

getMethod :: BC.ByteString
getMethod = "GET"

getCollectionRequest :: BC.ByteString -> Request
getCollectionRequest playerName = buildRequest hostName "GET" $ mconcat ["/xmlapi2", "/collection?username=", playerName]

