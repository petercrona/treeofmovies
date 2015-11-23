{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Dispatcher(dispatch)
import Database.MongoDB(connect, Pipe, host)
import Control.Monad.Trans (liftIO)
import Network.HTTP.Types (status500)
import Data.Text
import Dsl.ResourceAccess
import Dsl.DatabaseInfrastructure
import Dsl.Resource
import Dsl.Handler
import Resource.ResourceParser

type DB = Pipe

application :: [(String, HandlerExpr)] -> DB -> Application
application handlerMappings db request respond = do
  dispatch handlerMappings db request >>= respond

main :: IO ()
main = do
  db <- connect (host "127.0.0.1")
  run 3000 $ middleware $ application (getResourceToHandler api) db

middleware :: Middleware
middleware = gzip def
            .validateToken

validateToken :: Middleware
validateToken app request sendResponse = do
  print $ "Validating token: " ++ (show (queryString request))
  app request sendResponse

api :: ResourceExpr
api =
           resource "movie"
                    generic
                    (admin <||> user)
                    (admin <||> user)
                    (admin <||> (user <&&> confirmed))
                    (admin <||> user)
                    mysql
           `also`
           resource "user"
                    generic
                    (admin <||> (user <&&> confirmed))
                    (admin <||> user)
                    (admin <||> user)
                    (admin <||> user)
                    (memcache `backedBy` (mongoDb +++ mysql))
           `also`
           resource "session"
                    (specialized "Session handler")
                    guest
                    (admin <||> user)
                    (admin <||> user)
                    (admin <||> user)
                    redis
           `also`
           resource "cars"
                    (
                      (
                        generic
                        `atTheSameTime` (specialized "update number of cars added today")
                      )
                      `afterThat` (specialized "Update users cars")
                    )
                    (user <&&> admin)
                    (admin <||> user)
                    (admin <||> user)
                    (admin <||> user)
                    mysql
