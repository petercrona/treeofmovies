{-# LANGUAGE GADTs #-}

module Dsl.DatabaseInfrastructure
  ( DbExpr
  , mysql, redis, mongoDb, memcache
  , (+++), backedBy
  , evalDb
  ) where

-- Writes and reads to memcache but also writes to MySQL and MongoDB in a seperate thread
-- Memcache backedBy (MySQL ++ MongoDb)
--

data DbExpr where
  MySQL :: DbExpr
  Redis :: DbExpr
  MongoDB :: DbExpr
  Memcache :: DbExpr

  (:+++) :: DbExpr -> DbExpr -> DbExpr
  BackedBy :: DbExpr -> DbExpr -> DbExpr

  deriving (Eq)

instance Show DbExpr where
  show MySQL = "MySQL"
  show Redis = "Redis"
  show MongoDB = "MongoDB"
  show Memcache = "Memcache"
  show e = evalDb e

mysql :: DbExpr
mysql = MySQL

redis :: DbExpr
redis = Redis

mongoDb :: DbExpr
mongoDb = MongoDB

memcache :: DbExpr
memcache = Memcache

(+++) :: DbExpr -> DbExpr -> DbExpr
(+++) = (:+++)

backedBy :: DbExpr -> DbExpr -> DbExpr
backedBy = BackedBy

evalDb :: DbExpr -> String
evalDb (e1 :+++ e2) = (evalDb e1) ++ " and " ++ (evalDb e2)
evalDb (e1 `BackedBy` e2) = (evalDb e1) ++ " backed by " ++ (evalDb e2)
evalDb dbExpr = show dbExpr
