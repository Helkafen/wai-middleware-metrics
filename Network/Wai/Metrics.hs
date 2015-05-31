{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Metrics (
  initCounter,
  metrics) where

import Network.Wai
import System.Metrics
import qualified System.Metrics.Counter as Counter

initCounter :: IO Counter.Counter
initCounter = do
  store <- newStore
  requests <- createCounter "wai_request_count" store
  return requests

metrics :: Counter.Counter -> Middleware
metrics counter app req respond = do
  Counter.inc counter
  app req respond
