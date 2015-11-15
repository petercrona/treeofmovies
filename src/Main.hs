{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Dispatcher(dispatch)
import Database.MongoDB
import Control.Monad.Trans (liftIO)

application :: Pipe -> Application
application pipe request respond = do
  dispatch request pipe >>= respond

main :: IO ()
main = do
  pipe <- connect (host "127.0.0.1")
  run 3000 $ gzip def (application pipe)
