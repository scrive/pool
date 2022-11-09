# resource-pool-0.4.0.0 (???)
* Allow user to configure the number of stripes explicitly. [#15](https://github.com/scrive/pool/pull/15)
* Do not exceed maximum resources. [#16](https://github.com/scrive/pool/pull/16)

# resource-pool-0.3.1.0 (2022-06-15)
* Add `tryWithResource` and `tryTakeResource`.

# resource-pool-0.3.0.0 (2022-06-01)
* Rewrite based on `Control.Concurrent.QSem` for better throughput and latency.
* Make release of resources asynchronous exceptions safe.
* Remove dependency on `monad-control`.
* Expose the `.Internal` module.
* Add support for introspection.
* Add `PoolConfig`.
