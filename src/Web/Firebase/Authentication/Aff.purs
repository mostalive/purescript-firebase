module Web.Firebase.Authentication.Aff (Authenticator, authWithCustomToken) where


import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Prelude (Unit, ($>), (<<<))
import Web.Firebase.Aff (convertError)
import Web.Firebase.Authentication.Eff as AE
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types (DatabaseImpl)
import Web.Firebase.Types as FBT

type AuthToken = Auth


makeAff' :: forall t2 t3 t5. ((Error -> Eff t3 Unit) -> (t2 -> Eff t3 Unit) -> Eff t3 t5) -> Aff t3 t2
makeAff' oldAff = makeAff \k -> oldAff (k <<< Left) (k <<< Right) $> nonCanceler

authWithCustomToken :: forall eff.
                       String ->
                       Auth ->
                       Aff (firebase :: FBT.FirebaseEff | eff) Foreign
authWithCustomToken token ref = makeAff' (\errorCb successCb -> AE.authWithCustomToken token successCb (convertError errorCb) ref)

newtype Authenticator = Authenticator {token :: AuthToken, db :: DatabaseImpl}
mkAuthenticator :: AuthToken -> DatabaseImpl -> Authenticator
mkAuthenticator token db = Authenticator {token, db}

{-
foreign import _authWithCustomToken :: forall eff. Authenticator -> EffFnAff (firebase :: FBT.FirebaseEff) Foreign

authWithCustomToken :: forall eff. Authenticator -> Aff (firebase :: FBT.FirebaseEff) Foreign
authWithCustomToken = fromEffFnAff <<< _authWithCustomToken
-}
