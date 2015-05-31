{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Metrics (
  initCounter,
  metrics) where

import Network.Wai
import System.Metrics
import Control.Monad (when)
import qualified System.Metrics.Counter as Counter
import Network.HTTP.Types.Status (statusIsServerError)

initCounter :: IO (Counter.Counter, Counter.Counter)
initCounter = do
  store <- newStore
  requests <- createCounter "wai_request_count" store
  serverErrors <- createCounter "wai_server_error_count" store
  return (requests, serverErrors)

metrics :: Counter.Counter -> Counter.Counter -> Middleware
metrics counter errorCounter app req respond = do
  Counter.inc counter
  app req respond'
    where respond' :: Response -> IO ResponseReceived
          respond' res = do
            when (statusIsServerError $ responseStatus res) (Counter.inc errorCounter)
            respond res
