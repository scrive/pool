# resource-pool-0.4.0.0 (2023-01-16)
* Require `poolMaxResources` to be not smaller than the number of stripes.
* Add support for setting the number of stripes.
* Hide the constructor of `PoolConfig` from the public API and provide
  `defaultPoolConfig` so that future additions to `PoolConfig` don't require
  major version bumps.

# resource-pool-0.3.1.0 (2022-06-15)
* Add `tryWithResource` and `tryTakeResource`.

# resource-pool-0.3.0.0 (2022-06-01)
* Rewrite based on `Control.Concurrent.QSem` for better throughput and latency.
* Make release of resources asynchronous exceptions safe.
* Remove dependency on `monad-control`.
* Expose the `.Internal` module.
* Add support for introspection.
* Add `PoolConfig`.
