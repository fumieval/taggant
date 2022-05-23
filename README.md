Usage
----

Taggants are metadata passed around via the `X-TAGGANT` http header.
You can improve the tracability by logging its contents

Apply `Taggant.middleware` to your WAI application to keep track of taggants.

```haskell
import Taggant qualified

application = Taggant.middleware Taggant.defaultConfig $ ...
```

If a taggant is present, the middleware stores it in the Request's vault. Apply `fromWaiRequest` to the request to read the taggant.

```haskell
fromWaiRequest :: Request -> Maybe Context
```