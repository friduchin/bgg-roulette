module Config (getConfigValue, ConfigName (..)) where
-- env config

import qualified Data.Map as Map
import Configuration.Dotenv
import Data.Char
import Control.Error

getConfigValue :: ConfigName -> IO (Either String String)
getConfigValue name = loadFile defaultConfig >>= (return . f)
    where f config = note ("Missing field in config " ++ show name) $ Map.lookup (configTranslator name) $ Map.fromList config


-- names must be same as in .env file
data ConfigName = User 
                | Filename
                | Port
    deriving Show

configTranslator :: ConfigName -> String
configTranslator configName = toLower <$> show configName