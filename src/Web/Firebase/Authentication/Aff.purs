module Web.Firebase.Authentication.Aff (authWithCustomToken) where


import Control.Monad.Aff (Aff, makeAff)
import Data.Foreign (Foreign)
import Web.Firebase.Aff (convertError)
import Web.Firebase.Authentication.Eff as AE
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types as FBT

authWithCustomToken :: forall eff.
                       String ->
                       Auth ->
                       Aff (firebase :: FBT.FirebaseEff | eff) Foreign
authWithCustomToken token ref = makeAff (\errorCb successCb -> AE.authWithCustomToken token successCb (convertError errorCb) ref)
