{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Resource.Generic where

import Data.Maybe
import Data.Aeson
import Network.Wai
import Network.HTTP.Types (status200, status500)
import Data.Text(Text)
import Network.HTTP.Types.Method
import Db.MongoDB
import qualified Db.Driver as Entity
import GHC.Generics
import Database.MongoDB(Pipe)
import Data.AesonBson
import Data.Bson
import Text.Regex.Posix
import Data.Text(pack, unpack)
import Data.ByteString.Lazy(ByteString)

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
     getGetResponse entity
  where get' args = do
          case id of
            Right Nothing -> Entity.getAll MongoDbDriver entityName db
            Right (Just idVal) -> Entity.get MongoDbDriver entityName idVal db
            Left x -> return Nothing
        id = safeGetId request

getGetResponse :: Maybe ByteString -> IO Response
getGetResponse Nothing = returnError "Invalid request"
getGetResponse payLoad = return $ responseLBS
       status200
       [("Content-Type", "application/json")]
       (fromJust payLoad)

returnError :: ByteString -> IO Response
returnError x =
  return $ responseLBS
      status500
      [("Content-Type", "application/json")]
      x

create :: Text -> Request -> Pipe -> IO Response
create entityName request db = do
  document <- getRequestBodyAsDocument request
  id' <- Entity.create MongoDbDriver entityName document db
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      (fromJust id')


update :: Text -> Request -> Pipe -> IO Response
update entityName request db = do
  document <- getRequestBodyAsDocument request
  case id of
    Right (Just idVal) -> do
      id' <- Entity.update MongoDbDriver entityName idVal document db
      return $ responseLBS
        status200
        [("Content-Type", "application/json")]
        (fromJust id')
    Right Nothing -> do
      return $ responseLBS
        status200
        [("Content-Type", "application/json")]
        "FAIL"

  where id = safeGetId request

delete :: Text -> Request -> Pipe -> IO Response
delete entityName request db = do
  removedEntityResponse <- (Entity.remove MongoDbDriver entityName id db)
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      removedEntityResponse

  where id = getId request

getId :: Request -> Text
getId request = (pathInfo request) !! 1

safeGetId :: Request -> Either String (Maybe Text)
safeGetId request =
  let args = pathInfo request in
  case (length args > 1) of
      False -> Right Nothing
      True -> getIdIfValid $ args !! 1

getIdIfValid :: Text -> Either String (Maybe Text)
getIdIfValid id =
  case ((unpack id) =~ (unpack "^[a-fA-F0-9]+$") :: Bool) of
      True -> Right $ Just id
      False -> Left "Invalid id"

getRequestBodyAsDocument :: Request -> IO Document
getRequestBodyAsDocument request = do
  body <- strictRequestBody request
  return $ fromJust $ fmap bsonify (decode (body))
