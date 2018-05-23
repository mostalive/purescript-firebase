module Test.Authentication (authenticationSpec) where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Prelude (Unit, discard, ($))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.AdminSDK (CustomToken)
import Web.Firebase.Authentication.Aff as AuthAff
import Web.Firebase.Authentication.Eff (unAuth)
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types (App)
import Web.Firebase.Types as FBT

authenticationSpec :: forall eff. App -> Auth -> CustomToken -> Spec (firebase :: FBT.FirebaseEff, exception :: EXCEPTION | eff ) Unit
authenticationSpec app auth token = do
    describe "Authentication" do
      it "unauthenticates when not authenticated" do
        liftEff $ unAuth auth
      describe "with Aff" do
        it "does not throw an exception" do
          -- we can't fake authentication, CustomTokens can only be constructed by accessing lib
          -- also there is no return value in the firebase library when there is no error
          AuthAff.authWithCustomToken token app
