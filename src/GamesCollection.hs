module GamesCollection where
-- Parse stored xml collection and returns random game from it

import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Data.ByteString.Lazy as B
import Data.Maybe
import Text.XML.Light.Lexer
import System.Random

parsing :: IO ()
parsing = do
    collectionData <- B.readFile "collection.xml"
    let gamesList = getGamesList collectionData
    nr <- getRandomGame gamesList
    let game = gamesList !! nr
    print game

getGamesList :: XmlSource s => s -> [String]
getGamesList collectionData = fmap strContent names where
    elems = onlyElems $ parseXML collectionData
    items = Prelude.concatMap (findElements (QName "item" Nothing Nothing)) elems
    existingItems = Prelude.filter isItemExist items
    names = Prelude.concatMap (findElements (QName "name" Nothing Nothing)) existingItems

isItemExist :: Element -> Bool
isItemExist item = Prelude.foldr (\a b -> b && (fromMaybe "0" a == "1")) True attr
    where
        elems = findElements (QName "status" Nothing Nothing) item
        attr = fmap (findAttr (QName "own" Nothing Nothing)) elems

getRandomGame :: [a] -> IO Int
getRandomGame gamesList = randomGameNr
    where
        gamesCount = Prelude.length gamesList
        randomGameNr = randomRIO (0, gamesCount)
