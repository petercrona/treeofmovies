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
import Data.Text(unpack)

dbName :: Database
dbName = "baseball"

update :: Text -> Text -> Document -> Pipe -> IO (Maybe ByteString)
update entityName id entity db = do
  res <- access db master dbName (query entityName entity)
  return $ return $ encode $ show res

  where query entityName entity =
          findAndModify (select ["_id" =: (ObjId (read (unpack id))) ] entityName) entity

create :: Text -> Document -> Pipe -> IO (Maybe ByteString)
create entityName entity db = do
  res <- access db master dbName (query entityName entity)
  return $ return $ encode $ show res

  where query entityName entity =
          insert entityName entity

remove :: Text -> Text -> Pipe -> IO ByteString
remove entityName id db = do
  access db master dbName (query entityName id)
  return (encode $ True)

  where query entityName id =
          delete (select ["_id" =: (ObjId (read (unpack id))) ] entityName)

get :: Text -> Text -> Pipe -> IO (Maybe ByteString)
get entityName id db = do
  entity <- access db master dbName (query entityName id)
  return $ return $ encode (map aesonify entity)

  where query entityName id =
          rest =<< find (select ["_id" =: (ObjId (read (unpack id))) ] entityName)

getAll :: Text -> Pipe -> IO (Maybe ByteString)
getAll entityName db = do
  entity <- access db master dbName (allTeams)
  return $ return $ encode (map aesonify entity)

allTeams :: Action IO [Document]
allTeams = rest =<< find (select [] "team") {sort = ["home.city" =: 1]}
