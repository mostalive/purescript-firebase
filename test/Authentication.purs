module Test.Authentication (authenticationSpec) where

import Effect.Class (liftEffect)
import Prelude (Unit, discard, ($))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (expectError)
import Web.Firebase.Authentication.Aff as AuthAff
import Web.Firebase.Authentication.Eff (unAuth)
import Web.Firebase.Authentication.Types (Auth)

authenticationSpec :: Auth -> Spec Unit
authenticationSpec auth = do
    describe "Authentication" do
      it "unauthenticates when not authenticated" do
        liftEffect $ unAuth auth
      describe "with Aff" do
        it "on fake authentication throws an error" do
          expectError $ AuthAff.authWithCustomToken "faketoken" auth
