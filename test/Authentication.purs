module Test.Authentication (authenticationSpec) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Prelude (Unit, bind, discard, ($))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.Authentication.Aff as AuthAff
import Web.Firebase.Authentication.Eff (unAuth)
import Web.Firebase.Types as FBT
import Web.Firebase.UnsafeRef (refFor)

authenticationSpec :: forall eff. Spec (firebase :: FBT.FirebaseEff | eff ) Unit
authenticationSpec = do
    describe "Authentication" do
      it "unauthenticates when not authenticated" do
        r <- forbiddenRef
        liftEff $ unAuth r

      describe "with Aff" do
        it "on fake authentication throws an error" do
          r <- forbiddenRef
          expectError $ AuthAff.authWithCustomToken "faketoken" r

forbiddenRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
forbiddenRef = refFor "https://purescript-spike.firebaseio.com/forbidden"
