{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Db.Driver(DbDriver(..)) where

import Data.Text(Text, intercalate)
import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.ByteString.Lazy(ByteString)
import Data.AesonBson
import Data.Text(unpack)

class DbDriver a where
  dbName :: a -> Database
  update :: a -> Text -> Text -> Document -> Pipe -> IO (Maybe ByteString)
  create :: a -> Text -> Document -> Pipe -> IO (Maybe ByteString)
  remove :: a -> Text -> Text -> Pipe -> IO ByteString
  get    :: a -> Text -> Text -> Pipe -> IO (Maybe ByteString)
  getAll :: a -> Text -> Pipe -> IO (Maybe ByteString)
