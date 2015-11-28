{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setOnException, Settings)
import Network.Wai.Middleware.Gzip (gzip, def)
import Dispatcher(dispatch)
import Database.MongoDB(connect, Pipe, host, close)
import Control.Monad.Trans (liftIO)
import Network.HTTP.Types (status500)
import Data.Text
import Dsl.ResourceAccess
import Dsl.DatabaseInfrastructure
import Dsl.Resource
import Dsl.Handler
import Resource.ResourceParser
import Control.Exception

type DB = Pipe

application :: [(String, HandlerExpr)] -> Application
application handlerMappings request respond = do
  bracket
    (connect (host "127.0.0.1"))
    (\i -> close i)
    (\db -> dispatch handlerMappings db request >>= respond)

main :: IO ()
main = start

start :: IO()
start = do
  runSettings getSettings $ middleware $ application (getResourceToHandler api)

getSettings :: Settings
getSettings = do
  let settings = defaultSettings in
    setOnException exceptionHandler settings

exceptionHandler :: Maybe Request -> SomeException -> IO()
exceptionHandler _ ex = putStrLn $ show ex

middleware :: Middleware
middleware = gzip def
            .validateToken
            .loadRoles

validateToken :: Middleware
validateToken app request sendResponse = do
  print $ "Validating token: " ++ (show (queryString request))
  app request sendResponse

loadRoles :: Middleware
loadRoles app request sendResponse = do
  print $ "Loading roles based on token: " ++ (show (queryString request))
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
                    (
                      specialized "Session handler"
                     `afterThat` (specialized "Update user's last login")
                    )
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
