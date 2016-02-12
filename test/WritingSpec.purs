module Test.WritingSpec (writingSpec) where

import Prelude (Unit, bind, ($), class Show)

import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Aff.AVar (AVAR(), AVar, makeVar, takeVar, putVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), message)
import Data.Maybe (Maybe(Nothing))
import Data.Either (Either(Right))
import Web.Firebase as FB
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, pending, it, Spec())
import Test.Spec.Runner           (Process())
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import PlayWithFire (Success(Success), snapshot2success)
import Data.Foreign as F

entriesRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
entriesRef = refFor "https://purescript-spike.firebaseio.com/entries"


setRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
setRef = refFor "https://purescript-spike.firebaseio.com/entries/wecanset/arbitrary/paths"


writingSpec ::  forall eff. Spec ( avar :: AVAR, firebase :: FBT.FirebaseEff, err :: EXCEPTION | eff) Unit
writingSpec = do
  describe "Writing" do
      it "can add an item to a list" do
        location <- entriesRef
        newChildRef <- liftEff $ FB.push (F.toForeign $ {success: "random numbered string"}) Nothing location
        snap <- onceValue newChildRef
        (D.key snap) `shouldNotEqual` Nothing
        -- key is different on every write. Checking unique keys is work for QuickCheck
        (snapshot2success snap) `shouldEqual` (Right (Success {success: "random numbered string"}))
        -- use key to read value

      it "can overwrite an existing item" do
        let secondValue = {success: "second value"}
        location <- entriesRef
        newChildRef <- liftEff $ FB.push (F.toForeign $ {success: "initial value"}) Nothing location
        _ <- liftEff $ FB.set (F.toForeign $ secondValue) Nothing newChildRef
        snap <- onceValue newChildRef
        (snapshot2success snap) `shouldEqual` (Right (Success secondValue))
      it "pushE calls back with Nothing when no error occurs" do
          location <- entriesRef
          respVar  <- makeVar
          handle  <- liftEff $ FB.pushE (F.toForeign {some: "object"}) (\Nothing -> launchAff $ putVar respVar Nothing) location
          actual :: Maybe FBT.FirebaseErr <- takeVar respVar
          actual `shouldEqual` Nothing
      it "setE calls back with Nothing when no error occurs" do
          location <- setRef
          respVar  <- makeVar
          handle  <- liftEff $ FB.setE (F.toForeign {some: "object"}) (\Nothing -> launchAff $ putVar respVar Nothing) location
          actual :: Maybe FBT.FirebaseErr <- takeVar respVar
          actual `shouldEqual` Nothing

      pending "can overwrite an existing item in Aff"
      pending "can add a server-side timestamp to new items"
      pending "push Aff when writing to non-existant location returns an error"
      pending "Removal confirmed by subscription on()" -- baby steps
      pending "Removal not notified after subscription turned off()" -- test with timeout? how?
      -- implement AFF with error callback (it is error object or nothing, so we can make it 'or Right "write successful", which we can reuse in a value writeSuccess so we can assert against that. Not sure how to combine that with the value of the key that is also returned from the js function'
