module Test.Misc where

import Prelude (Unit, bind, ($), (>>=))

import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Right))
import Web.Firebase as FB
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, pending, it, Spec())
import Test.Spec.Runner           (Process())
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import PlayWithFire (readSuccessAff, Success(Success), snapshot2success)
import Data.Foreign as F


getRoot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
getRoot = refFor "https://purescript-spike.firebaseio.com/"

entriesRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
entriesRef = refFor "https://purescript-spike.firebaseio.com/entries"

rootSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
rootSnapshot = getRoot >>= onceValue

entriesSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
entriesSnapshot = getRoot >>= \r -> (liftEff $ FB.child "entries" r) >>= onceValue

readRootChild :: String -> forall eff. Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
readRootChild location = getRoot >>= childAt >>= readSuccessAff
  where
    childAt ref = liftEff $ FB.child location ref

getYes :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
getYes = readRootChild "entries/-K7GbWeFHfJXlun7szRe"

-- Reading non-existant data will just wait forever
-- because it might exist in the future?
-- can only be tested with a timeout?
childAddedError :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
childAddedError = readRootChild "doesnotexist"

miscSpec ::  forall eff. Spec ( process :: Process, firebase :: FBT.FirebaseEff | eff) Unit
miscSpec = do
  describe "Firebase" do
    describe "Reading once" do
      pending "delivers a value when asked"
      pending "can check for the existence of a child of a firebase ref"
      it "delivers a child when added" do
        success <- getYes
        success `shouldEqual` (Right (Success {success: "yes!"}))


