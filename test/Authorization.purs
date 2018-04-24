module Test.Authorization where

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (message)
import Data.Either (either)
import Data.Foreign (toForeign)
import Prelude (Unit, bind, discard, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase (EventType(ChildMoved, ChildChanged, ChildRemoved, ChildAdded))
import Web.Firebase.Aff (mkEventAtLocation, mkSaveable)
import Web.Firebase.Aff as FAff
import Web.Firebase.Types as FBT

forbiddenR :: forall eff. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
forbiddenR = FAff.child "forbidden"

authorizationSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff | eff) Unit
authorizationSpec ref = do
  describe "Authorization" do
    describe "Writing" do
      it "with Aff push on forbidden location throws an error" do
        forbiddenRef <- forbiddenR ref
        let newValue = {success: "push Aff"}
        expectError $ FAff.push $ mkSaveable (toForeign newValue) forbiddenRef
    describe "once() on forbidden location" do
      it "with Aff throws an error" do
          forbiddenRef <- forbiddenR ref
          e <- attempt $ FAff.onceValue forbiddenRef  -- catch error thrown and assert
          either (\err -> (message err) `shouldEqual` "permission_denied at /forbidden: Client doesn't have permission to access the desired data.\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e
    describe "on() at forbidden location" do
      it "ChildAdded with Aff throws an error" do
        forbiddenRef <- forbiddenR ref
        expectError $ FAff.on $ mkEventAtLocation ChildAdded forbiddenRef
      it "ChildRemoved with Aff throws an error" do
        forbiddenRef <- forbiddenR ref
        expectError $ FAff.on $ mkEventAtLocation ChildRemoved forbiddenRef
      it "ChildChanged with Aff throws an error" do
        forbiddenRef <- forbiddenR ref
        expectError $ FAff.on $ mkEventAtLocation ChildChanged forbiddenRef
      it "ChildMoved with Aff throws an error" do
        forbiddenRef <- forbiddenR ref
        expectError $ FAff.on $ mkEventAtLocation ChildMoved forbiddenRef
    it "set() with Aff at forbidden location throws an error" do
      forbiddenRef <- forbiddenR ref
      let newValue = {success: "set Aff"}
      e <- attempt $ FAff.set $ mkSaveable (toForeign newValue) forbiddenRef
      either (\err -> (message err) `shouldEqual` "PERMISSION_DENIED: Permission denied\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e
