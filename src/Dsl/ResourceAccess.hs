{-# LANGUAGE GADTs #-}

module Dsl.ResourceAccess
  ( RoleExpr
  , admin, user, guest, confirmed
  , (<&&>), (<||>)
  , eval
  ) where

data RoleExpr where
  Admin :: RoleExpr
  User :: RoleExpr
  Guest :: RoleExpr
  Confirmed :: RoleExpr

  (:<||>) :: RoleExpr -> RoleExpr -> RoleExpr
  (:<&&>) :: RoleExpr -> RoleExpr -> RoleExpr

  deriving (Eq)

instance Show RoleExpr where
  show = showRoleExpr

admin :: RoleExpr
admin = Admin

user :: RoleExpr
user = User

guest :: RoleExpr
guest = Guest

confirmed :: RoleExpr
confirmed = Confirmed

(<||>) :: RoleExpr -> RoleExpr -> RoleExpr
(<||>) = (:<||>)

(<&&>) :: RoleExpr -> RoleExpr -> RoleExpr
(<&&>) = (:<&&>)

eval :: [RoleExpr] -> RoleExpr -> Bool
eval r (x :<||> y) = eval r x || eval r y
eval r (x :<&&> y) = eval r x && eval r y
eval r (r') = any (r'==) r

showRoleExpr :: RoleExpr -> String
showRoleExpr (x :<||> y) = "(" ++ showRoleExpr x ++ " or " ++ showRoleExpr y ++ ")"
showRoleExpr (x :<&&> y) = "(" ++ showRoleExpr x ++ " and " ++ showRoleExpr y ++ ")"
showRoleExpr Admin = "Admin"
showRoleExpr User = "User"
showRoleExpr Guest = "Guest"
showRoleExpr Confirmed = "Confirmed"
