{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Metrics (
  addWaiMetrics,
  WaiMetrics(..),
  metrics) where

import Network.Wai
import System.Metrics
import Control.Monad (when)
import Data.Time.Clock
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import Network.HTTP.Types.Status (statusIsServerError)

data WaiMetrics = WaiMetrics {
  requestCounter :: Counter.Counter
 ,serverErrorCounter :: Counter.Counter
 ,responseTimeDistribution :: Distribution.Distribution
}

addWaiMetrics :: Store -> IO WaiMetrics
addWaiMetrics store = do
  req <- createCounter "wai_request_count" store
  err <- createCounter "wai_server_error_count" store
  tim <- createDistribution "wai_response_time_distribution" store
  return $ WaiMetrics req err tim

metrics :: WaiMetrics -> Middleware
metrics waiMetrics app req respond = do
  Counter.inc (requestCounter waiMetrics)
  start <- getCurrentTime
  app req (respond' start)
    where respond' :: UTCTime -> Response -> IO ResponseReceived
          respond' start res = do
            when (statusIsServerError $ responseStatus res) (Counter.inc (serverErrorCounter waiMetrics))
            end <- getCurrentTime
            Distribution.add (responseTimeDistribution waiMetrics) (realToFrac $ diffUTCTime end start)
            respond res
