module Test.Authorization where

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (message)
import Control.Monad.State.Trans (StateT)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Identity (Identity)
import Prelude (Unit, bind, discard, ($))
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase (EventType(ChildMoved, ChildChanged, ChildRemoved, ChildAdded))
import Web.Firebase.Aff as FAff
import Web.Firebase.Types as FBT
import Web.Firebase.UnsafeRef (refFor)

forbiddenR :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
forbiddenR = refFor "https://purescript-spike.firebaseio.com/forbidden"

 --authorizationSpec :: forall eff. FBT.Firebase -> StateT (Array (Group(Aff (firebase :: FBT.FirebaseEff | eff) Unit ))) Identity Unit
authorizationSpec :: forall t11.
   StateT
     (Array
        (Group
           (Aff
              ( firebase :: FBT.FirebaseEff
              | t11
              )
              Unit
           )
        )
     )
     Identity
     Unit
authorizationSpec = do
    describe "Authorization" do
      let blork = "no effects"
      describe "Writing" do
        it "with Aff push on forbidden location throws an error" do
          forbiddenRef <- forbiddenR
          let newValue = {success: "push Aff"}
          expectError $ FAff.push (toForeign newValue) forbiddenRef
      describe "once() on forbidden location" do
        it "with Aff throws an error" do
           forbiddenRef <- forbiddenR
           e <- attempt $ FAff.onceValue forbiddenRef  -- catch error thrown and assert
           either (\err -> (message err) `shouldEqual` "permission_denied at /forbidden: Client doesn't have permission to access the desired data.\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e
      describe "on() at forbidden location" do
        it "ChildAdded with Aff throws an error" do
          forbiddenRef <- forbiddenR
          expectError $ FAff.on ChildAdded forbiddenRef
        it "ChildRemoved with Aff throws an error" do
          forbiddenRef <- forbiddenR
          expectError $ FAff.on ChildRemoved forbiddenRef
        it "ChildChanged with Aff throws an error" do
          forbiddenRef <- forbiddenR
          expectError $ FAff.on ChildChanged forbiddenRef
        it "ChildMoved with Aff throws an error" do
          forbiddenRef <- forbiddenR
          expectError $ FAff.on ChildMoved forbiddenRef
      it "set() with Aff at forbidden location throws an error" do
        forbiddenRef <- forbiddenR
        let newValue = {success: "set Aff"}
        e <- attempt $ FAff.set (toForeign newValue) forbiddenRef
        either (\err -> (message err) `shouldEqual` "PERMISSION_DENIED: Permission denied\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e

{-   describe "Firebase API breaks its documentation" do
       it "once() throws an exeption when no error callback provided and an error occurs" do
          -- I prefer handling errors, but this is the firebase api...
          handle <- liftEff $ once ChildAdded (\_ -> pure unit) Nothing forbiddenRef
          "shoul get here" `shouldEqual` "should get here"
-}
