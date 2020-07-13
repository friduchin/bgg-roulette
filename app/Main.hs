module Main where

import Bgg
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as L
import GamesCollection
import Control.Monad

main :: IO ()
main = do
    putStrLn "Please input bgg username"
    name <- getLine
    response <- httpLBS $ getCollectionRequest $ BC.pack name
    result <- storeCollection response
    when result GamesCollection.parsing

storeCollection :: Response L.ByteString -> IO Bool
storeCollection response = do
    let status = getResponseStatusCode response
    if status == 200
        then do
            let body = getResponseBody response
            L.writeFile "collection.xml" body
            return True
        else do
            print "status incorrect"
            return False