module GamesCollection (needUpdate, parsing, storeCollection) where
-- Parse stored xml collection and returns random game from it

import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Text.XML.Light.Lexer
import qualified Data.ByteString.Lazy as B
import System.Random
import Data.Maybe (fromMaybe)
import Config
import Data.Either (fromRight)
import Data.Monoid (mconcat)
import Data.DateTime
import Data.List.Split (splitOn)
import Data.List
import System.Directory


oneDay :: Integer
oneDay = 24 * 60 * 60


getCollectionFilename :: IO String
getCollectionFilename = head . filter (isPrefixOf "collection") <$> listDirectory "collection"


getTimestamp :: IO Integer
getTimestamp = do
    fileName <- getCollectionFilename
    return $ read $ head $ splitOn "." $ head $ tail $ splitOn "_" fileName


needUpdate :: IO Bool
needUpdate = do
    oldTimestamp <- getTimestamp
    currentTime <- toSeconds <$> getCurrentTime
    return $ currentTime > (oldTimestamp + oneDay)


parsing :: IO String
parsing = do
    filename <- getCollectionFilename
    collectionData <- B.readFile $ mconcat ["collection/", filename]
    let gamesList = getGamesList collectionData
    getRandomElem gamesList


getGamesList :: XmlSource s => s -> [String]
getGamesList collectionData = fmap strContent names where
    elems = onlyElems $ parseXML collectionData
    items = concatMap (findElements (QName "item" Nothing Nothing)) elems
    existingItems = filter isItemExist items
    names = concatMap (findElements (QName "name" Nothing Nothing)) existingItems


isItemExist :: Element -> Bool
isItemExist item = foldr (\a b -> b && (fromMaybe "0" a == "1")) True attr
    where
        elems = findElements (QName "status" Nothing Nothing) item
        attr = fmap (findAttr (QName "own" Nothing Nothing)) elems


getRandomElem :: [a] -> IO a
getRandomElem list = randomRIO (0, length list - 1) >>= (\x -> return $ list !! x)


storeCollection content = do
    timestamp <- toSeconds <$> getCurrentTime
    filename <- getConfigValue Filename
    oldFiles <- filter (isPrefixOf "collection") <$> listDirectory "collection"
    mapM_ (\x -> removeFile $ mconcat ["collection/", x]) oldFiles
    B.writeFile (mconcat ["collection/", fromRight "" filename, "_", show timestamp, ".xml"]) content
