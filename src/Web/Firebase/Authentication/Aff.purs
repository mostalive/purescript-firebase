module Web.Firebase.Authentication.Aff (authWithCustomToken) where


import Control.Monad.Aff (Aff(), makeAff)
import Data.Foreign (Foreign)

import Web.Firebase.Types as FBT
import Web.Firebase.Aff (convertError)
import Web.Firebase.Authentication.Eff as AE

authWithCustomToken :: forall eff.
                       String ->
                       FBT.Firebase ->
                       Aff (firebase :: FBT.FirebaseEff | eff) Foreign
authWithCustomToken token ref = makeAff (\errorCb successCb -> AE.authWithCustomToken token successCb (convertError errorCb) ref)
