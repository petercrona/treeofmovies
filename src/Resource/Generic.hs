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
import Data.ByteString.Char8
import Database.MongoDB(Pipe)

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
  do entity <- Entity.get entityName "fdsafd" db
     return $ responseLBS
       status200
       [("Content-Type", "application/json")]
       (fromJust entity)

create :: Text -> Request -> Pipe -> IO Response
create entityName request db =
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      "POST"

update :: Text -> Request -> Pipe -> IO Response
update entityName request db =
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      "PUT"

delete :: Text -> Request -> Pipe -> IO Response
delete entityName request db =
  return $ responseLBS
      status200
      [("Content-Type", "application/json")]
      "DELETE"
