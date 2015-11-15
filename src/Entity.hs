{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Entity where

import Data.Text(Text, intercalate)
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString.Lazy(ByteString)
import Data.AesonBson

update :: String -> String -> a -> IO Bool
update entityName id entity' = return True

create :: String -> a -> IO (Maybe Integer)
create entityName entity = return $ Just 4

delete :: String -> String -> IO Bool
delete entityName id = return True

get :: Text -> Text -> Pipe -> IO (Maybe ByteString)
get entityName id db = do
  fdsa <- access db master "baseball" (allTeams)
  return $ return $ encode (map aesonify fdsa)

allTeams :: Action IO [Document]
allTeams = rest =<< find (select [] "team") {sort = ["home.city" =: 1]}
