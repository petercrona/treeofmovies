{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Db.MongoDB(MongoDbDriver(MongoDbDriver)) where

import Db.Driver
import Data.Text(Text, intercalate)
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString.Lazy(ByteString)
import Data.AesonBson
import Data.Text(unpack)

data MongoDbDriver = MongoDbDriver

instance DbDriver MongoDbDriver where
  dbName = dbName'
  update = update'
  create = create'
  remove = remove'
  get = get'
  getAll = getAll'

dbName' :: MongoDbDriver -> Database
dbName' _ = "baseball"

update' :: MongoDbDriver -> Text -> Text -> Document -> Pipe -> IO (Maybe ByteString)
update' driver entityName id entity db = do
  res <- access db master (dbName' driver) (query entityName entity)
  return $ return $ encode $ show res

  where query entityName entity =
          findAndModify (select ["_id" =: (ObjId (read (unpack id))) ] entityName) entity

create' :: MongoDbDriver -> Text -> Document -> Pipe -> IO (Maybe ByteString)
create' driver entityName entity db = do
  res <- access db master (dbName' driver) (query entityName entity)
  return $ return $ encode $ show res

  where query entityName entity =
          insert entityName entity

remove' :: MongoDbDriver -> Text -> Text -> Pipe -> IO ByteString
remove' driver entityName id db = do
  access db master (dbName' driver) (query entityName id)
  return (encode $ True)

  where query entityName id =
          delete (select ["_id" =: (ObjId (read (unpack id))) ] entityName)

get' :: MongoDbDriver -> Text -> Text -> Pipe -> IO (Maybe ByteString)
get' driver entityName id db = do
  entity <- access db master (dbName' driver) (query entityName id)
  return $ return $ encode (map aesonify entity)

  where query entityName id =
          rest =<< find (select ["_id" =: (ObjId (read (unpack id))) ] entityName)

getAll' :: MongoDbDriver -> Text -> Pipe -> IO (Maybe ByteString)
getAll' driver entityName db = do
  entity <- access db master (dbName' driver) (query entityName)
  return $ return $ encode (map aesonify entity)

  where query entityName = rest =<< find (select [] entityName)
