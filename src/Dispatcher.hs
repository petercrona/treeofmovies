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

dispatch :: [(String, HandlerExpr)] -> Pipe -> Request -> IO Response
dispatch handlerMapping db req = case (lookup (unpack resourceName) handlerMapping) of
  Just handler -> genericResource resourceName req db
  Nothing -> get404Response

  where resourceName = head $ pathInfo req

get404Response :: IO Response
get404Response = return $ responseLBS
                 status404
                 [("Content-Type", "application/json")]
                 "{\"message\": \"The resource does not exist\"}"
