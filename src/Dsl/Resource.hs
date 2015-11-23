{-# LANGUAGE GADTs #-}

module Dsl.Resource
  ( ResourceExpr
  , resource
  , also
  , getResourceToHandler
  ) where

import Dsl.DatabaseInfrastructure
import Dsl.ResourceAccess
import Dsl.Handler
import Data.Char

data ResourceExpr where
  Resource :: String ->
              HandlerExpr ->
              RoleExpr ->
              RoleExpr ->
              RoleExpr ->
              RoleExpr ->
              DbExpr ->
              ResourceExpr

  And :: ResourceExpr -> ResourceExpr -> ResourceExpr

  deriving (Eq)

instance Show ResourceExpr where
  show = printResource

resource :: String ->
            HandlerExpr ->
            RoleExpr ->
            RoleExpr ->
            RoleExpr ->
            RoleExpr ->
            DbExpr ->
            ResourceExpr
resource = Resource

also :: ResourceExpr -> ResourceExpr -> ResourceExpr
also = And

getResourceToHandler :: ResourceExpr -> [(String, HandlerExpr)]
getResourceToHandler (And e1 e2) = getResourceToHandler e1 ++ getResourceToHandler e2
getResourceToHandler (Resource n handler _ _ _ _ _) = [(n, handler)]

printResource :: ResourceExpr -> String
printResource (And e1 e2) = (printResource e1) ++ "\n" ++ (printResource e2)
printResource (Resource (n:ns) handler rc rr ru rd db) =
  "\nResource "
  ++ toUpper n : ns
  ++ "\n\n\t"
  ++ show handler
  ++ "\n\n\t"
  ++ "Create allowed for users with roles: "
  ++ show rc
  ++ "\n\tRead allowed for users with roles: "
  ++ show rr
  ++ "\n\tUpdate allowed for users with roles: "
  ++ show ru
  ++ "\n\tDelete allowed for users with roles: "
  ++ show rd
  ++ "\n\n\tPersisted using: "
  ++ show db
