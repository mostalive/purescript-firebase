module Web.Firebase.Authentication.Eff (
    authWithCustomToken
  , authWithOAuthRedirect
  , authWithOAuthRedirectSilent
  , onAuth
  , offAuth
  , unAuth
) where

import Prelude (Unit(), pure, unit)
import Web.Firebase.Types (Firebase(), FirebaseErr)
import Web.Firebase.Authentication.Types (Auth)
import Effect (Effect())
import Foreign (Foreign())

import Data.Function.Uncurried (Fn4, Fn3, Fn2, Fn1, runFn4, runFn3, runFn2, runFn1)

foreign import _onAuth :: Fn2 (Foreign -> Effect Unit) Firebase (Effect Unit)

onAuth :: (Foreign -> Effect Unit) -> Firebase -> Effect Unit
onAuth = runFn2 _onAuth

-- | de-register an onAuth callback
--https://www.firebase.com/docs/web/api/firebase/offauth.html
-- for sign out functionality, see unAuth
foreign import _offAuth :: Fn2 Firebase (Foreign -> Effect Unit) (Effect Unit)

offAuth :: Firebase -> (Foreign -> Effect Unit) -> Effect Unit
offAuth = runFn2 _offAuth

-- we should also have a runFn1 for offSimple (or maybe offBasic, or offRef since we only care about tthe ref and nothing else)

-- | oAuth identification, redirecting to a provider (e.g. "twitter").
-- No idea what can be in the Error, and since it is a redirect, how is the callback going to happen?
-- perhaps only when the provider is not configured correctly in firebase?
foreign import _authWithOAuthRedirect :: Fn3
                                         String
                                         (Foreign -> Effect Unit)
                                         Firebase
                                         (Effect Unit)

type AuthenticationProvider = String

authWithOAuthRedirect :: String ->
                         (Foreign -> Effect Unit) ->
                         Firebase ->
                         (Effect Unit)
authWithOAuthRedirect = runFn3 _authWithOAuthRedirect


noOpCallBack :: forall a. a -> Effect Unit
noOpCallBack _ = pure unit

-- | For those cases (always?) where listening for the error callback does not make sense
-- subscribing to authStatus is more relevant.
authWithOAuthRedirectSilent :: String ->
                         Firebase ->
                         (Effect Unit)
authWithOAuthRedirectSilent provider = authWithOAuthRedirect provider noOpCallBack

-- | authenticate a 'client' (most often a server or test laptop) with a custom token (JWT)
-- https://www.firebase.com/docs/web/guide/login/custom.html
-- Instead of an on error and on failure paramater that may or may not be null,
-- we pass an error and success callback, so that making an Aff function is easier
-- and we don't have to worry about null / maybe on the purescript side
foreign import _authWithCustomToken :: Fn4
                        String
                        (Foreign -> Effect Unit)
                        (FirebaseErr -> Effect Unit)
                        Auth
                        (Effect Unit)

type AuthToken = String
type AuthData = Foreign -- we are not parsing this just yet, TBD

authWithCustomToken :: AuthToken ->
                       (Foreign -> Effect Unit) ->
                       (FirebaseErr -> Effect Unit) ->
                       Auth ->
                       Effect Unit
authWithCustomToken = runFn4 _authWithCustomToken

-- | sign out of the application
--
foreign import _unAuth :: Fn1 Auth (Effect Unit)

unAuth :: Auth -> Effect Unit
unAuth = runFn1 _unAuth
