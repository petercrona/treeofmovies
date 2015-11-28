{-# LANGUAGE OverloadedStrings #-}

module Dispatcher (dispatch) where

import Data.Text(Text)
import Data.Text(unpack)
import Network.Wai
import Network.HTTP.Types (status200, status404)
import Resource.Generic
import Database.MongoDB(Pipe)
import Control.Monad.Trans
import Dsl.Handler
import Control.Exception

dispatch :: [(String, HandlerExpr)] -> Pipe -> Request -> IO Response
dispatch handlerMapping db req = runHandler handlerMapping resourceName db req
  where resourceName = safeHead $ pathInfo req

runHandler :: [(String, HandlerExpr)] -> Maybe Text -> Pipe -> Request -> IO Response
runHandler handlerMapping Nothing _ _ = get404Response
runHandler handlerMapping (Just resourceName) db req =
  case (lookup (unpack resourceName) handlerMapping) of
      Just handler -> genericResource resourceName req db
      Nothing -> get404Response

get404Response :: IO Response
get404Response = return $ responseLBS
                 status404
                 [("Content-Type", "application/json")]
                 "{\"message\": \"The resource does not exist\"}"

safeHead :: [e] -> Maybe e
safeHead [] = Nothing
safeHead (x:xs) = Just x
