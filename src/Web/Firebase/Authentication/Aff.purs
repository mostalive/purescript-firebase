module Web.Firebase.Authentication.Aff (authWithCustomToken) where


import Prelude (Unit)
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff.Exception (Error())
import Data.Foreign (Foreign)

import Web.Firebase as FB
import Web.Firebase.Types as FBT
import Web.Firebase.Monad.Aff (convertError)
import Web.Firebase.Authentication.Eff as AE

-- Unit for now, should be authData, or at least foreign.
authWithCustomToken :: forall eff.
                       String ->
                       FBT.Firebase ->
                       Aff (firebase :: FBT.FirebaseEff | eff) Foreign
authWithCustomToken token ref = makeAff (\errorCb successCb -> AE.authWithCustomToken token successCb (convertError errorCb) ref)
