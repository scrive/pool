# resource-pool-0.5.0.0 (2025-??-??)
* Drop support for GHC < 8.10.
* Use STM based lockless implementation as it results in much better throughput
  in a multi-threaded environment when number of stripes is not equal to the
  number of capabilities (in particular with a single stripe).
* Stop running resource freeing functions within `uninterruptibleMask`.
* `destroyResource` no longer ignores exceptions thrown from resource releasing
  functions.
* Change the default number of stripes to 1.
* Do not exceed the maximum number of resources if the number of stripes does
  not divide it.

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
