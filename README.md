Usage
----

Taggants are metadata passed around via the `X-TAGGANT` http header.

Apply `Taggant.middleware` to your WAI application to keep track of taggants.

```haskell
import Taggant qualified

application = Taggant.middleware $ ...
```

This middleware stores the taggant in the Request's [vault](https://hackage.haskell.org/package/wai-3.2.3/docs/Network-Wai.html#v:vault). Apply `fromWaiRequest` to read the taggant.

```haskell
fromWaiRequest :: Request -> Taggant
```

You can pass the taggant to the logger, and/or other applications so that the trace of the request chain is more visible.