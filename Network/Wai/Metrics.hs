{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Metrics (
  addWaiMetrics,
  WaiMetrics(..),
  metrics) where

import Network.Wai
import System.Metrics
import Control.Monad (when)
import qualified System.Metrics.Counter as Counter
import Network.HTTP.Types.Status (statusIsServerError)

data WaiMetrics = WaiMetrics {
  requestCounter :: Counter.Counter
 ,serverErrorCounter :: Counter.Counter
}

addWaiMetrics :: Store -> IO WaiMetrics
addWaiMetrics store = do
  req <- createCounter "wai_request_count" store
  err <- createCounter "wai_server_error_count" store
  return $ WaiMetrics req err

metrics :: WaiMetrics -> Middleware
metrics waiMetrics app req respond = do
  Counter.inc (requestCounter waiMetrics)
  app req respond'
    where respond' :: Response -> IO ResponseReceived
          respond' res = do
            when (statusIsServerError $ responseStatus res) (Counter.inc (serverErrorCounter waiMetrics))
            respond res
