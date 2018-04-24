module Web.Firebase.Aff.Read
(
  onceValue
, valueAt
) where
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Data.Foreign (Foreign)
import Prelude (pure, bind, ($), (<<<))
import Web.Firebase as FB
import Web.Firebase.Aff (mkEventAtLocation, once)
import Web.Firebase.DataSnapshot (val)
import Web.Firebase.Types as FBT

-- | Inspired by its Eff relative.
-- | Throw takes a message and throws a MonadError in Aff with that message
-- TODO move to its own module, also used by 'regular' Aff module
throw :: forall eff a. String -> Aff eff a
throw = throwError <<< error

valueAt :: forall eff. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) Foreign
valueAt ref = do
       snap <- onceValue ref
       pure $ (val snap)

onceValue :: forall e. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
onceValue root = once $ mkEventAtLocation FB.Value root
