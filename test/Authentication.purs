module Test.Authentication (authenticationSpec) where

import Prelude (Unit, bind, ($))

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.Authentication.Eff (unAuth)
import Web.Firebase.Authentication.Aff as AuthAff

authenticationSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff, err :: EXCEPTION, console :: CONSOLE | eff ) Unit
authenticationSpec forbiddenRef = do
    describe "Authentication" do
      it "unauthenticates when not authenticated" do
        liftEff $ unAuth forbiddenRef

      describe "with Aff" do
        it "on fake authentication throws an error" do
          expectError $ AuthAff.authWithCustomToken "faketoken" forbiddenRef
