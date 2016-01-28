module Test.Main where

import Prelude (Unit, bind, ($), (>>=))

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Right))
import Web.Firebase as FB
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (Process(), run)
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import PlayWithFire (readSuccessAff, Success(Success), snapshot2success)
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
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
      it "can tell us the number of children" do
	rs <- rootSnapshot
	let numChildren = D.numChildren rs
        numChildren `shouldEqual` 1
      it "can tell us a child does not exist" do
	rs <- rootSnapshot
	let noChild = D.hasChild rs "doesnotexist"
	noChild `shouldEqual` false
      it "can tell us a child exists" do
	rs <- rootSnapshot
	let childExists = D.hasChild rs "entries" -- type Key = String ?
	childExists `shouldEqual` true
      it "can tell us the location at the snapshot exists" do
	rs <- rootSnapshot
        let rootExists = D.exists rs
	rootExists `shouldEqual` true
      it "can tell us it has children" do
        rs <- rootSnapshot
        let hasChildren = D.hasChildren rs
        hasChildren `shouldEqual` true

      it "says the key of the database root is Nothing" do
        rs <- rootSnapshot
        let key = D.key rs
        key `shouldEqual` Nothing

      it "says the key of /entries is entries" do
        sn <- entriesSnapshot
        let key = D.key sn
        key `shouldEqual` (Just "entries")

      pending "it can not tell us the location at the snapshot does not exist"
      pending "can it say the value of child \"entries\" is Nothing?"
        -- this relies on trying to read a firebase ref with once, and that 'works' by never being called back
      pending "it can give us a snapshot of one of its children"
      -- implement forEach callback that returns true after first call, so it terminates
      -- put that callback into Aff, so we can wait for the result
      -- with Aff, and the Aff variable, we can retrieve a list.
    describe "Authorization" do
	 {- https://www.firebase.com/docs/web/guide/user-auth.html#section-handling-errors:
  All errors are Error objects containing at least code and message attributes. In some cases, additional information will be provided via the details attribute. For example:
  {
	    code: "TRANSPORT_UNAVAILABLE",
       message: "There are no login transports available for the requested method.",
         details: "More details about the specific error here."
 }
	-}
      pending "returns an error object subscribing to an unauthorized location"
      -- this forces us to handle errors in Aff, and parse Error objects
      -- as well as placing some authorization rules in Firebase
      -- documentation on firebase Error was hard to find, having an actual one would allow us to write some marshalling code. code and message should be present, details wrapped in a Maybe. Given we don't own this interface, placing a console.log in the FFi javascript side is wise for now.
  describe "Writing" do
      it "can add an item to a list" do
        -- see writeWithFire (or rather: addWithFire)
        location <- entriesRef
        newChildRef <- liftEff $ FB.push (F.toForeign $ {success: "random numbered string"}) Nothing location
        snap <- onceValue newChildRef
        (D.key snap) `shouldNotEqual` Nothing
        -- key is different on every write. Checking unique keys is work for QuickCheck
        (snapshot2success snap) `shouldEqual` (Right (Success {success: "random numbered string"}))
        -- use key to read value

      pending "can overwrite a (possibly) existing item"
      pending "can add a server-side timestamp to new items"
