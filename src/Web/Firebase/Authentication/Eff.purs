module Web.Firebase.Authentication.Eff (
  authWithCustomToken
  , authWithOAuthRedirect
  , onAuth
  , authWithOAuthRedirectSilent
) where

import Prelude (Unit(), pure, unit)
import Web.Firebase.Types (Firebase(), FirebaseEff(), FirebaseErr)
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe)
import Data.Foreign (Foreign())

import Data.Function (Fn3, Fn2, runFn3, runFn2)

foreign import _onAuth :: forall eff. Fn2 (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit) Firebase (Eff (firebase :: FirebaseEff | eff) Unit)

onAuth :: forall eff. (Foreign -> Eff ( firebase :: FirebaseEff | eff) Unit) -> Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
onAuth = runFn2 _onAuth

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

-- | authenticate a 'client' (most often a server or test laptop) with a custom token (JWT)
-- https://www.firebase.com/docs/web/guide/login/custom.html
foreign import _authWithCustomToken :: forall eff. Fn3
                        String
                        ((Maybe FirebaseErr) -> Eff (firebase :: FirebaseEff | eff ) Unit)
                        Firebase
                        (Eff (firebase :: FirebaseEff | eff) Unit)

type AuthToken = String

authWithCustomToken :: forall eff.
                       AuthToken ->
                       ((Maybe FirebaseErr) -> Eff (firebase :: FirebaseEff | eff ) Unit) ->
                       Firebase ->
                       Eff (firebase :: FirebaseEff | eff) Unit
authWithCustomToken = runFn3 _authWithCustomToken
