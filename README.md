# wai-metrics
A WAI middleware to collect the following [EKG](https://ocharles.org.uk/blog/posts/2012-12-11-24-day-of-hackage-ekg.html) metrics of compatible web servers:
- number of requests (counter `wai.request_count`)
- number of server errors (counter `wai.server_error_count`)
- latency distribution (distribution `wai.latency_distribution`)

Typical usage, with Scotty (compile with GHC option `-with-rtsopts=-T` for GC metrics):
```
import Web.Scotty
import Control.Applicative
import System.Remote.Monitoring (serverMetricStore, forkServer)
import Network.Wai.Metrics

main :: IO()
main = do
  store <- serverMetricStore <$> forkServer "localhost" 8000
  waiMetrics <- registerWaiMetrics store
  scotty 3000 $ do
    middleware (metrics waiMetrics)
    get "/" $ html "Ping"
```

See wai-metrics-scotty-demo.hs for a full demo.
