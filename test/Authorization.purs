module Test.Authorization where

import Prelude (Unit, bind, ($), map, show, pure)

import Control.Monad.Aff (forkAff,later', launchAff, attempt)
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Aff.AVar (AVAR(), makeVar, takeVar, putVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), message)
import Control.Alt ((<|>))
import Data.Either (either)
import Data.Maybe (Maybe(Just))
import Data.Foreign (toForeign)
import Web.Firebase.Types as FBT
import Web.Firebase (EventType(..),once, pushE, push)
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.Monad.Aff as FAff

authorizationSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff, err :: EXCEPTION, avar :: AVAR | eff ) Unit
authorizationSpec forbiddenRef = do
    describe "Understanding AVar and Par" do
      it "can write to a var" do
        respVar <- makeVar
        handle <- forkAff (later' 100 $ putVar respVar true)
        actual <- takeVar respVar
        actual `shouldEqual` true
      it "can race two vars manually" do
        respVar <- makeVar
        handle <- forkAff (later' 100 $ putVar respVar "fast")
        handleSlow <- forkAff (later' 200 $ putVar respVar "slow")
        actual <- takeVar respVar
        actual `shouldEqual` "fast"
      it "can race two vars with an alternative" do
        let fast = (later' 100 $ pure "fast")
            slow = (later' 200 $ pure "slow")
        actual <- runPar (Par fast <|> Par slow)
        actual `shouldEqual` "fast"
    describe "Authorization" do
      describe "Writing" do
        it "pushE() on forbidden location calls error callback" do
          respVar <- makeVar
          handle  <- liftEff $ pushE (toForeign {some: "object"}) (\err -> launchAff $ putVar respVar (map show err)) forbiddenRef
          actual <- takeVar respVar
          actual `shouldEqual` Just "PERMISSION_DENIED: Permission denied\n | firebase code: | \n PERMISSION_DENIED"
        it "push() on forbidden location calls error callback" do
          respVar <- makeVar
          handle  <- liftEff $ push (toForeign {some: "object"}) (Just (Just (\err -> launchAff $ putVar respVar (show err)))) forbiddenRef
          actual <- takeVar respVar
          actual `shouldEqual` "PERMISSION_DENIED: Permission denied\n | firebase code: | \n PERMISSION_DENIED"
        it "with Aff push on forbidden location throws an error" do
          let newValue = {success: "push Aff"}
          expectError $ FAff.push forbiddenRef (toForeign newValue)
      describe "once() on forbidden location" do
        it "with Eff calls an error callback" do
          respVar <- makeVar
          handle  <- liftEff $ once ChildAdded (\snap -> launchAff $ putVar respVar "unexpected sucess") (\_ -> launchAff $ putVar respVar "child forbidden") forbiddenRef
          actual <- takeVar respVar
          actual `shouldEqual` "child forbidden"
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
        e <- attempt $ FAff.set forbiddenRef (toForeign newValue)
        either (\err -> (message err) `shouldEqual` "PERMISSION_DENIED: Permission denied\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e

{-   describe "Firebase API breaks its documentation" do
       it "once() throws an exeption when no error callback provided and an error occurs" do
          -- I prefer handling errors, but this is the firebase api...
          handle <- liftEff $ once ChildAdded (\_ -> pure unit) Nothing forbiddenRef
          "shoul get here" `shouldEqual` "should get here"
-}
