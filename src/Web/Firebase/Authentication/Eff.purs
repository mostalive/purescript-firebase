module Web.Firebase.Authentication.Eff (
    authWithCustomToken
  , authWithOAuthRedirect
  , authWithOAuthRedirectSilent
  , onAuth
  , offAuth
  , unAuth
) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)
import Prelude (Unit, pure, unit)
import Web.Firebase.AdminSDK (CustomToken)
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types (Firebase, FirebaseEff, FirebaseErr, App)

foreign import _onAuth :: forall eff. Fn2 (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit) Firebase (Eff (firebase :: FirebaseEff | eff) Unit)

onAuth :: forall eff. (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit) -> Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
onAuth = runFn2 _onAuth

-- | de-register an onAuth callback
--https://www.firebase.com/docs/web/api/firebase/offauth.html
-- for sign out functionality, see unAuth
foreign import _offAuth :: forall eff. Fn2 Firebase (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit) (Eff (firebase :: FirebaseEff | eff) Unit)

offAuth :: forall eff. Firebase -> (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit) -> Eff (firebase :: FirebaseEff | eff) Unit
offAuth = runFn2 _offAuth

-- we should also have a runFn1 for offSimple (or maybe offBasic, or offRef since we only care about tthe ref and nothing else)

-- | oAuth identification, redirecting to a provider (e.g. "twitter").
-- No idea what can be in the Error, and since it is a redirect, how is the callback going to happen?
-- perhaps only when the provider is not configured correctly in firebase?
foreign import _authWithOAuthRedirect :: forall eff. Fn3
                                         String
                                         (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit)
                                         Firebase
                                         (Eff (firebase :: FirebaseEff | eff) Unit)

type AuthenticationProvider = String

authWithOAuthRedirect :: forall eff.
                         String ->
                         (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit) ->
                         Firebase ->
                         (Eff (firebase :: FirebaseEff | eff) Unit)
authWithOAuthRedirect = runFn3 _authWithOAuthRedirect


noOpCallBack :: forall eff a. a -> Eff (eff) Unit
noOpCallBack _ = pure unit

-- | For those cases (always?) where listening for the error callback does not make sense
-- subscribing to authStatus is more relevant.
authWithOAuthRedirectSilent :: forall eff.
                         String ->
                         Firebase ->
                         (Eff (firebase :: FirebaseEff | eff) Unit)
authWithOAuthRedirectSilent provider = authWithOAuthRedirect provider noOpCallBack

foreign import authWithCustomTokenImpl :: forall eff. Fn2 CustomToken App (Eff (firebase :: FirebaseEff, exception :: EXCEPTION | eff ) Unit)

type AuthToken = String
type AuthData = Foreign -- we are not parsing this just yet, TBD

authWithCustomToken :: forall eff. CustomToken -> App -> (Eff (firebase :: FirebaseEff, exception :: EXCEPTION | eff ) Unit)
authWithCustomToken = runFn2 authWithCustomTokenImpl
-- | sign out of the application
--
foreign import _unAuth :: forall eff. Fn1 Auth (Eff (firebase :: FirebaseEff | eff) Unit)

unAuth :: forall eff. Auth -> Eff (firebase :: FirebaseEff | eff ) Unit
unAuth = runFn1 _unAuth
