module Web.Firebase.UnsafeRef
  ( refFor,
    unsafeRef
  ) where
import Prelude (($))
import Effect.Aff (Aff())
import Effect (Effect())
import Effect.Class (liftEffect)
import Web.Firebase as FB
import Web.Firebase.Types as FBT

--| Aff version of unsafeRef
refFor :: String -> Aff FBT.Firebase
refFor s = liftEffect (unsafeRef s)

-- | Unsafely read a string that might be a reference, and turn it into a firebase reference
-- may throw javascript exceptions, intended to keep test code terse, not meant for production usage
-- This will fail silently if you pass something that is not a url.
unsafeRef :: String -> Effect FBT.Firebase
unsafeRef s = FB.newFirebase $ s
