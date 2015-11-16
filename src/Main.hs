{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Dispatcher(dispatch)
import Database.MongoDB(connect, Pipe, host)
import Control.Monad.Trans (liftIO)
import Network.HTTP.Types (status500)
import Data.Text
import Auth

type DB = Pipe

application :: DB -> Application
application pipe request respond = do
  dispatch request pipe >>= respond

main :: IO ()
main = do
  db <- connect (host "127.0.0.1")
  run 3000 $ middleware $ application db

middleware :: Middleware
middleware = gzip def
            .validateToken

validateToken :: Middleware
validateToken app request sendResponse = do
  print $ "Validating token: " ++ (show (queryString request))
  app request sendResponse
  --sendResponse $ responseLBS status500 [] "Hello World"

type InMemory = Bool
type Name = String
data Resource = Resource Name Auth InMemory

getResource :: Resource
getResource = Resource "movie" ((admin <||> user) <&&> confirmed) True

admin = Auth $ \r -> r == "ADMIN"
confirmed = Auth $ \r -> r == "CONFIRMED"
user = Auth $ \r -> r == "USER"
