# wai-metrics
WAI middleware that collects requests metrics.

Provides a WAI middleware that feeds the following EKG Counters.
- number of requests (counter `wai_request_count`)
- number of server errors (counter `wai_server_error_count`)

Typical usage, with Scotty:
```
main :: IO()
main = do
  store <- newStore                  -- Create an EKG store
  waiMetrics <- addWaiMetrics store  -- Register both counters in EKG
  scotty 3000 $ do
    middleware (metrics waiMetrics)  -- Add the middleware to Scotty
    get "/" $ html "Ping"
```

See wai-metrics-scotty-demo.hs in git for a full demo.
