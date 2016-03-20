module Test.WritingSpec (writingSpec) where

import Prelude (Unit, (>>=), ($), bind, show)

import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Aff.AVar (AVAR(), makeVar, takeVar, putVar)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(Nothing))
import Data.Either (Either(Right))
import Web.Firebase as FB
import Web.Firebase.Monad.Aff as FAff
import Web.Firebase.UnsafeRef (refFor)
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, pending, it, Spec())
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import PlayWithFire (Success(Success), snapshot2success)
import Data.Foreign as F
import Data.Foreign.Class (readWith)

entriesRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
entriesRef = refFor "https://purescript-spike.firebaseio.com/entries"


setRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
setRef = refFor "https://purescript-spike.firebaseio.com/entries/wecanset/arbitrary/paths"

writingSpec ::  forall eff. Spec ( avar :: AVAR, firebase :: FBT.FirebaseEff, err :: EXCEPTION | eff) Unit
writingSpec = do
  describe "Writing" do
    describe "with Eff" do
      it "can add an item to a list" do
        location <- entriesRef
        newChildRef <- liftEff $ FB.push (F.toForeign $ {success: "random numbered string"}) Nothing location
        snap <- FAff.onceValue newChildRef
        (D.key snap) `shouldNotEqual` Nothing
        -- key is different on every write. Checking unique keys is work for QuickCheck
        (snapshot2success snap) `shouldEqual` (Right (Success {success: "random numbered string"}))
        -- use key to read value

      it "can overwrite an existing item" do
        let secondValue = {success: "second value"}
        location <- entriesRef
        newChildRef <- liftEff $ FB.push (F.toForeign $ {success: "initial value"}) Nothing location
        _ <- liftEff $ FB.set (F.toForeign $ secondValue) Nothing newChildRef
        snap <- FAff.onceValue newChildRef
        (snapshot2success snap) `shouldEqual` (Right (Success secondValue))
      it "pushE calls back with Nothing when no error occurs" do
          location <- entriesRef
          respVar  <- makeVar
          handle  <- liftEff $ FB.pushE (F.toForeign {some: "object"}) (\err -> launchAff $ putVar respVar err) location
          actual :: Maybe FBT.FirebaseErr <- takeVar respVar
          actual `shouldEqual` Nothing
      it "setE calls back with Nothing when no error occurs" do
          location <- setRef
          respVar  <- makeVar
          liftEff $ FB.setE (F.toForeign {some: "object"}) (\err -> launchAff $ putVar respVar err) location
          actual :: Maybe FBT.FirebaseErr <- takeVar respVar
          actual `shouldEqual` Nothing
      pending "can add a server-side timestamp to new items"

      pending "Removal confirmed by subscription on()" -- baby steps
      pending "Removal not notified after subscription turned off()" -- test with timeout? how?

    describe "with Aff" do
      it "push with Aff returns a firebase key" do
        location <- setRef
        let newValue = {success: "push Aff"}
        newChildRef <- FAff.push location (F.toForeign newValue)
        writtenValue <- FAff.valueAt newChildRef
        (readWith show writtenValue) `shouldEqual` (Right (Success newValue))
      it "can overwrite an existing item in Aff" do
        location <- setRef
        let newValue = {success: "set Aff"}
        FAff.set location (F.toForeign newValue)
        writtenValue <- FAff.valueAt location
        (readWith show writtenValue) `shouldEqual` (Right (Success newValue))
