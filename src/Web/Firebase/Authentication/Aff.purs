module Web.Firebase.Authentication.Aff (Authenticator, authWithCustomToken) where


import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Data.Foreign (Foreign)
import Prelude ((<<<))
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types (DatabaseImpl)
import Web.Firebase.Types as FBT

type AuthToken = Auth

newtype Authenticator = Authenticator {token :: AuthToken, db :: DatabaseImpl}
mkAuthenticator :: AuthToken -> DatabaseImpl -> Authenticator
mkAuthenticator token db = Authenticator {token, db}

foreign import _authWithCustomToken :: forall eff. Authenticator -> EffFnAff (firebase :: FBT.FirebaseEff) Foreign

authWithCustomToken :: forall eff. Authenticator -> Aff (firebase :: FBT.FirebaseEff) Foreign
authWithCustomToken = fromEffFnAff <<< _authWithCustomToken
