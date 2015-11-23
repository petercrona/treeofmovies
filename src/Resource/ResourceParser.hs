module Resource.ResourceParser (getHandlers) where

import Dsl.Resource
import Dsl.Handler

-- Handler needs a way to be run, so that I can create a function which runs it
-- Resource needs a way to give it's name, so that I can use it for URLs

getHandlers :: ResourceExpr -> (String, HandlerExpr)
getHandlers = undefined
