module Test.Authorization where

import Prelude (Unit, bind, ($))

import Control.Apply ((*>))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (EXCEPTION(), message)
import Control.Alt ((<|>))
import Data.Either (either)
import Data.Foreign (toForeign)
import Web.Firebase.Types as FBT
import Web.Firebase (EventType(ChildMoved, ChildChanged, ChildRemoved, ChildAdded))
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.Monad.Aff as FAff

authorizationSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff, err :: EXCEPTION | eff ) Unit
authorizationSpec forbiddenRef = do
    describe "Authorization" do
      describe "Writing" do
        it "with Aff push on forbidden location throws an error" do
          let newValue = {success: "push Aff"}
          expectError $ FAff.push (toForeign newValue) forbiddenRef
      describe "once() on forbidden location" do
        it "with Aff throws an error" do
           e <- attempt $ FAff.onceValue forbiddenRef  -- catch error thrown and assert
           either (\err -> (message err) `shouldEqual` "permission_denied at /forbidden: Client doesn't have permission to access the desired data.\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e
      describe "on() at forbidden location" do
        it "ChildAdded with Aff throws an error" do
          expectError $ FAff.on ChildAdded forbiddenRef
        it "ChildRemoved with Aff throws an error" do
          expectError $ FAff.on ChildRemoved forbiddenRef
        it "ChildChanged with Aff throws an error" do
          expectError $ FAff.on ChildChanged forbiddenRef
        it "ChildMoved with Aff throws an error" do
          expectError $ FAff.on ChildMoved forbiddenRef
      it "set() with Aff at forbidden location throws an error" do
        let newValue = {success: "set Aff"}
        e <- attempt $ FAff.set (toForeign newValue) forbiddenRef
        either (\err -> (message err) `shouldEqual` "PERMISSION_DENIED: Permission denied\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e

{-   describe "Firebase API breaks its documentation" do
       it "once() throws an exeption when no error callback provided and an error occurs" do
          -- I prefer handling errors, but this is the firebase api...
          handle <- liftEff $ once ChildAdded (\_ -> pure unit) Nothing forbiddenRef
          "shoul get here" `shouldEqual` "should get here"
-}
