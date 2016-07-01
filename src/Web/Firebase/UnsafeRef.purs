module Web.Firebase.UnsafeRef
  ( refFor,
    unsafeRef
  ) where
import Prelude (($))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Web.Firebase as FB
import Web.Firebase.Types as FBT

--| Aff version of unsafeRef
refFor :: String -> forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
refFor s = liftEff (unsafeRef s)

-- | Unsafely read a string that might be a reference, and turn it into a firebase reference
-- may throw javascript exceptions, intended to keep test code terse, not meant for production usage
-- This will fail silently if you pass something that is not a url.
unsafeRef :: String -> forall eff. Eff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
unsafeRef s = FB.newFirebase $ s
