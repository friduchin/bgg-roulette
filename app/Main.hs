module Main where

import Bgg
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple
import GamesCollection
import Control.Monad
import Config
import Data.Either
import Web.Scotty
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans (lift)


main :: IO ()
main = do
    port <- getConfigValue Port
    scotty (read (fromRight "3000" port) :: Int) 
        $ get "/randomgame" $ do
            needToUpdate <- lift needUpdate
            when needToUpdate (lift (getConfigValue User >>= doCollectionRequest))
            game <- lift parsing
            html $ TL.pack game


doCollectionRequest :: Either String String -> IO ()
doCollectionRequest (Right username) = do
    print "request in progress"
    response <- httpLBS $ getCollectionRequest $ BC.pack username
    if getResponseStatusCode response == 200
        then storeCollection $ getResponseBody response 
        else print "response is incorrect"
doCollectionRequest (Left msg) = print msg