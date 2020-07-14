module GamesCollection (parsing) where
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

parsing :: IO ()
parsing = do
    collectionData <- getConfigValue Filename >>= B.readFile . fromRight ""
    let gamesList = getGamesList collectionData
    game <- getRandomGame gamesList
    print game

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

getRandomGame :: [a] -> IO a
getRandomGame gamesList = randomGameNr >>= (\x -> return $ gamesList !! x)
    where
        gamesCount = length gamesList
        randomGameNr = randomRIO (0, gamesCount - 1)
