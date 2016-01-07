module Test.Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class (liftEff)
import Data.Either
import Data.Either.Unsafe  (fromRight)
import Data.URI (runParseURI)
import qualified Web.Firebase as FB
import qualified Web.Firebase.DataSnapshot as D
import qualified Web.Firebase.Types as FBT
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (Process(), run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import PlayWithFire (readSuccessAff, Success(Success), onceAff) -- TODO move onceAff to Web.Firebase.Aff

getRoot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
getRoot = do
  let fbUri = fromRight $ runParseURI "https://purescript-spike.firebaseio.com/"
  liftEff $ FB.newFirebase fbUri

rootSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
rootSnapshot = do
  root <- getRoot
  s <- onceAff root
  pure s

getYes :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
getYes = do
  root <- getRoot
  entries <- liftEff $ FB.child "entries" root
  readSuccessAff entries

childAddedError :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
childAddedError = do
  root <- getRoot
  entries <- liftEff $ FB.child "doesnotexist" root
  readSuccessAff entries

main ::  forall eff. Eff ( console :: CONSOLE, process :: Process, firebase :: FBT.FirebaseEff | eff) Unit
main = run [consoleReporter] do
  describe "Firebase" do
    describe "Reading once" do
      it "delivers a child when added" do
        success <- getYes
        success `shouldEqual` (Right (Success {success: "yes!"}))
      pending "delivers a value when asked"
      pending "can check for the existence of a chile of a firebase ref"
    describe "DataSnapshot" do
      -- literal API
      -- the difference between snapshots and refs is somewhat confusing
      it "can tell us a child does not exist" do
	rs <- rootSnapshot
	let noChild = D.hasChild rs "doesnotexist"
	noChild `shouldEqual` false
 {-
      it "can tell us a child does not exist" do
	childExists <- D.hasChild rootSnapshot "doesnotexist"
	childExists `shouldEqual` true
 -}
    describe "Writing" do
      pending "can add an item to a list" -- see writeWithFire (or rather: addWithFire)
      pending "can overwrite a (possibly) existing item"
      pending "can add a server-side timestamp to new items"
