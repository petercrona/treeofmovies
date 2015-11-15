{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Resource.Generic where

import Data.Maybe
import Data.Aeson
import Network.Wai
import Network.HTTP.Types (status200)
import Data.Text(Text)
import Network.HTTP.Types.Method
import qualified Entity
import GHC.Generics
import Database.MongoDB(Pipe)
import Data.AesonBson
import Data.Bson

genericResource :: Text -> Request -> Pipe -> IO Response
genericResource entityName request db =
  routeOnVerb (requestMethod request) entityName request db

routeOnVerb :: Method -> Text -> Request -> Pipe -> IO Response
routeOnVerb "GET" = get
routeOnVerb "POST" = create
routeOnVerb "PUT" = update
routeOnVerb "DELETE" = delete

get :: Text -> Request -> Pipe -> IO Response
get entityName request db =
  do entity <- get' (pathInfo request)
     return $ responseLBS
       status200
       [("Content-Type", "application/json")]
       (fromJust entity)
  where get' args = case length args of
          1 -> Entity.getAll entityName db
          2 -> Entity.get entityName (getId request) db

create :: Text -> Request -> Pipe -> IO Response
create entityName request db = do
  document <- getRequestBodyAsDocument request
  id' <- Entity.create entityName document db
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      (fromJust id')


update :: Text -> Request -> Pipe -> IO Response
update entityName request db = do
  document <- getRequestBodyAsDocument request
  id' <- Entity.update entityName id document db
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      (fromJust id')

  where id = getId request

delete :: Text -> Request -> Pipe -> IO Response
delete entityName request db = do
  removedEntityResponse <- (Entity.remove entityName id db)
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      removedEntityResponse

  where id = getId request

getId :: Request -> Text
getId request = (pathInfo request) !! 1

getRequestBodyAsDocument :: Request -> IO Document
getRequestBodyAsDocument request = do
  body <- strictRequestBody request
  return $ fromJust $ fmap bsonify (decode (body))
