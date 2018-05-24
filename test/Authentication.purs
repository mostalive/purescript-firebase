module Test.Authentication (authenticationSpec) where

import Control.Monad.Eff.Exception (EXCEPTION)
import Prelude (Unit, bind)
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (fail)
import Web.Firebase.AdminSDK as Admin
import Web.Firebase.Types as FBT

authenticationSpec :: forall eff. Spec (firebase :: FBT.FirebaseEff, exception :: EXCEPTION | eff ) Unit
authenticationSpec = do
    describe "Authentication" do
--       it "unauthenticates when not authenticated" do
--        liftEff $ unAuth auth
      describe "with Aff" do
        it "does not throw an exception" do
          token <- Admin.everythingInJs adminFileName
          fail "can't check this"


adminFileName :: String
adminFileName = "purescript-spike-firebase-adminsdk-d11ou-52e3eedfd8.json"
