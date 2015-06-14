{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import System.Metrics (newStore)
import Network.Wai.Metrics
import Web.Scotty (scottyApp, middleware, get, html)
import Network.Wai (Application)
import Control.Monad (replicateM_, when)
import Data.Int (Int64)
import qualified System.Metrics.Counter as Counter
import qualified Network.Wai.Test as WT
import qualified Data.ByteString as BS

testServer :: Bool -> (Application -> IO a) -> Int -> IO WaiMetrics
testServer measure action times = do
  store <- newStore
  waiMetrics <- registerWaiMetrics store
  app <- scottyApp $ do
    when (measure) (middleware (metrics waiMetrics))
    get "/" $ html "Ping"
  replicateM_ times (action app)
  return waiMetrics

readRequestCounter :: Bool -> (Application -> IO a) -> Int -> IO Int64
readRequestCounter measure action times = do
  waiMetrics <- testServer measure action times
  Counter.read (requestCounter waiMetrics)

-- Send a GET request to a WAI Application
httpGet :: BS.ByteString -> Application -> IO WT.SResponse
httpGet path =  WT.runSession (WT.srequest (WT.SRequest req ""))
  where req = WT.setRawPathInfo WT.defaultRequest path

-- As to version 0.2.1, the middleware costs 0.7 microseconds per request
-- Most of this time is spent with the latency measure
main :: IO()
main = defaultMain [
  bgroup "fib" [
                 bench "with metrics"    $ nfIO (readRequestCounter True  (httpGet "") 10000)
               , bench "without metrics" $ nfIO (readRequestCounter False (httpGet "") 10000)
               ]
  ]
