{-# LANGUAGE GADTs #-}

-- generic afterThat specialized (...) afterThat specialized (...)
-- Add to movie-collection. Update movies in UserObj. Update DailyMovieAddStatistics
module Dsl.Handler
  ( HandlerExpr
  , generic, specialized
  , afterThat
  , atTheSameTime
  ) where

data HandlerExpr where
  Generic :: HandlerExpr
  Specialized :: String -> HandlerExpr
  AfterThat :: HandlerExpr -> HandlerExpr -> HandlerExpr
  AtTheSameTime :: HandlerExpr -> HandlerExpr -> HandlerExpr

  deriving (Eq)

instance Show HandlerExpr where
  show Generic = "Generic"
  show (Specialized s) = s
  show (AfterThat h1 h2) = "(" ++ show h1 ++ " after that " ++ show h2 ++ ")"
  show (AtTheSameTime h1 h2) = "(" ++ show h1 ++ " at the same time as " ++ show h2 ++ ")"

generic :: HandlerExpr
generic = Generic

specialized :: String -> HandlerExpr
specialized = Specialized

afterThat :: HandlerExpr -> HandlerExpr -> HandlerExpr
afterThat = AfterThat

atTheSameTime :: HandlerExpr -> HandlerExpr -> HandlerExpr
atTheSameTime = AtTheSameTime
