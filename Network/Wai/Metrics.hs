{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.Wai.Metrics
License     : BSD3
Stability   : experimental

A <http://hackage.haskell.org/package/wai WAI> middleware to collect the following <https://ocharles.org.uk/blog/posts/2012-12-11-24-day-of-hackage-ekg.html EKG> metrics from compatible web servers:

* number of requests (counter @wai.request_count@)
* number of response by status code, broken down class (count @wai.response_status_xxx@)
* latency distribution (distribution @wai.latency_distribution@)


Here's an example of reading these metrics from a Scotty server, and displaying them with EKG.

> -- Compile with GHC option `-with-rtsopts=-T` for GC metrics
> import Web.Scotty
> import Control.Applicative
> import System.Remote.Monitoring (serverMetricStore, forkServer)
> import Network.Wai.Metrics
>
> main :: IO()
> main = do
>   store <- serverMetricStore <$> forkServer "localhost" 8000
>   waiMetrics <- registerWaiMetrics store
>   scotty 3000 $ do
>     middleware (metrics waiMetrics)
>     get "/" $ html "Ping"

Now have a look at <http://localhost:8000 your local EKG instance> and display the request count by clicking on 'wai.request_count'.

WAI metrics can also be stored in a bare EKG store, with no UI and no GC metrics. Use ekg-core's newStore function.

Compatible web servers include the following:

*Yesod
*Scotty
*Spock
*Servant
*Warp
-}
module Network.Wai.Metrics (
  registerWaiMetrics,
  WaiMetrics(..),
  metrics) where

import Network.Wai
import System.Metrics
import Control.Applicative
import Data.Time.Clock
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution
import Network.HTTP.Types.Status (statusCode)

{-|
The metrics to feed in WAI and register in EKG.
-}
data WaiMetrics = WaiMetrics {
  requestCounter :: Counter.Counter
 ,latencyDistribution :: Distribution.Distribution
 ,statusCode100Counter :: Counter.Counter
 ,statusCode200Counter :: Counter.Counter
 ,statusCode300Counter :: Counter.Counter
 ,statusCode400Counter :: Counter.Counter
 ,statusCode500Counter :: Counter.Counter
}

{-|
Register in EKG a number of metrics related to web server activity.

* @wai.request_count@
* @wai.response_status_1xx@
* @wai.response_status_2xx@
* @wai.response_status_3xx@
* @wai.response_status_4xx@
* @wai.response_status_5xx@
* @wai.latency_distribution@
-}
registerWaiMetrics :: Store -> IO WaiMetrics
registerWaiMetrics store =
  WaiMetrics
    <$> createCounter "wai.request_count" store
    <*> createDistribution "wai.latency_distribution" store
    <*> createCounter "wai.response_status_1xx" store
    <*> createCounter "wai.response_status_2xx" store
    <*> createCounter "wai.response_status_3xx" store
    <*> createCounter "wai.response_status_4xx" store
    <*> createCounter "wai.response_status_5xx" store

{-|
Create a middleware to be added to a WAI-based webserver.
-}
metrics :: WaiMetrics -> Middleware
metrics waiMetrics app req respond = do
  Counter.inc (requestCounter waiMetrics)
  start <- getCurrentTime
  app req (respond' start)
    where respond' :: UTCTime -> Response -> IO ResponseReceived
          respond' start res = do
            Counter.inc $ case statusCode $ responseStatus res of
              s | s >= 500  -> statusCode500Counter waiMetrics
                | s >= 400  -> statusCode400Counter waiMetrics
                | s >= 300  -> statusCode300Counter waiMetrics
                | s >= 200  -> statusCode200Counter waiMetrics
                | otherwise -> statusCode100Counter waiMetrics
            end <- getCurrentTime
            Distribution.add (latencyDistribution waiMetrics) (realToFrac $ diffUTCTime end start)
            respond res
