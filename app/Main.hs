module Main where

import Bgg
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as L
import GamesCollection
import Control.Monad
import Config
import Data.Either
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Text.Lazy
import Control.Monad.Trans


main :: IO ()
main = do
    port <- getConfigValue Port
    scotty (read (fromRight "3000" port) :: Int) 
        $ get "/randomgame" $ do
            game <- lift parsing
            html $ pack game


main2 :: IO ()
main2 = getConfigValue User >>= doRequest


doRequest :: Either String String -> IO ()
doRequest (Left msg) = print msg
doRequest (Right username) = do
    response <- httpLBS $ getCollectionRequest $ BC.pack username
    result <- storeCollection response
    print result


storeCollection :: Response L.ByteString -> IO Bool
storeCollection response = do
    let status = getResponseStatusCode response
    if status == 200
        then do
            let body = getResponseBody response
            filename <- getConfigValue Filename
            L.writeFile (fromRight "" filename) body
            return True
        else do
            print "status incorrect"
            return False