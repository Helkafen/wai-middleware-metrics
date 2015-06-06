# wai-metrics
A [WAI](https://hackage.haskell.org/package/wai) middleware to collect the following [EKG](https://ocharles.org.uk/blog/posts/2012-12-11-24-day-of-hackage-ekg.html) metrics from compatible web servers:
- number of requests (counter `wai.request_count`)
- number of server errors (counter `wai.server_error_count`)
- latency distribution (distribution `wai.latency_distribution`)

Compatible web servers include the following: Yesod, Scotty, Spock, Servant, Warp.

Documentation and sample code [here](https://hackage.haskell.org/package/wai-metrics).
