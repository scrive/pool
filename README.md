# resource-pool

[![CI](https://github.com/scrive/pool/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/scrive/pool/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/resource-pool.svg)](https://hackage.haskell.org/package/resource-pool)
[![Stackage LTS](https://www.stackage.org/package/resource-pool/badge/lts)](https://www.stackage.org/lts/package/resource-pool)
[![Stackage Nightly](https://www.stackage.org/package/resource-pool/badge/nightly)](https://www.stackage.org/nightly/package/resource-pool)

A high-performance striped resource pooling implementation for Haskell based on
[QSem](https://hackage.haskell.org/package/base/docs/Control-Concurrent-QSem.html).

## Advice for library authors

If your library creates a pool on behalf of its users, don't expose your own,
restricted set of pool parameters (size, TTL, ...) and construct the
`PoolConfig` internally. Such a config inevitably lags behind features of this
library (stripe count, labels, whatever comes next) and users can't take
advantage of them without waiting for you to mirror each one.

Instead, take a function of type `IO a -> (a -> IO ()) -> PoolConfig a` as a
parameter. Your library supplies the resource creation and destruction actions:

```haskell
createConnectionPool
  :: ConnectionSettings
  -> (IO Connection -> (Connection -> IO ()) -> PoolConfig Connection)
  -> IO (Pool Connection)
createConnectionPool settings mkPoolConfig =
  newPool $ mkPoolConfig connect disconnect
  where
    connect :: IO Connection
    connect = ...

    disconnect :: Connection -> IO ()
    disconnect = ...
```

while users retain full control over the rest of the pool configuration:

```haskell
pool <- createConnectionPool settings $ \create free ->
  setPoolLabel "db"
    . setNumStripes (Just 1)
    $ defaultPoolConfig create free 60 10
```
