module Test.Authentication (authenticationSpec) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Prelude (Unit, bind, discard, ($))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.Aff (child)
import Web.Firebase.Authentication.Aff as AuthAff
import Web.Firebase.Authentication.Eff (unAuth)
import Web.Firebase.Types (Firebase)
import Web.Firebase.Types as FBT
import Web.Firebase.Authentication.Types (Auth)

authenticationSpec :: forall eff. Auth -> Spec (firebase :: FBT.FirebaseEff | eff ) Unit
authenticationSpec auth = do
    describe "Authentication" do
      it "unauthenticates when not authenticated" do
        liftEff $ unAuth auth
      describe "with Aff" do
        it "on fake authentication throws an error" do
          expectError $ AuthAff.authWithCustomToken "faketoken" auth
