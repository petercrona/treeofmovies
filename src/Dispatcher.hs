{-# LANGUAGE OverloadedStrings #-}

module Dispatcher (dispatch) where

import Data.Text(Text)
import Network.Wai
import Resource.Generic
import Database.MongoDB
import Control.Monad.Trans

dispatch :: Request -> Pipe -> IO Response
dispatch req db = case (pathInfo req) of

  ("user":args) -> genericResource "User" req db
  ("movie":args) -> genericResource "Movie" req db
  ("authentication":args) -> genericResource "AuthenticationSession" req db

  _ -> genericResource "Index" req db
