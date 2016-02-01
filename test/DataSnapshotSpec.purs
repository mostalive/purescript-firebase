module Test.DataSnapshotSpec (dataSnapshotSpec) where

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

entriesSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
entriesSnapshot = getRoot >>= \r -> (liftEff $ FB.child "entries" r) >>= onceValue

getRoot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
getRoot = refFor "https://purescript-spike.firebaseio.com/"

dataSnapshotSpec ::  forall eff. Spec ( process :: Process, firebase :: FBT.FirebaseEff | eff) Unit
dataSnapshotSpec = do
    describe "DataSnapshot" do
      -- literal API
      -- the difference between snapshots and refs is somewhat confusing
      it "can tell us the number of children" do
        rs <- entriesSnapshot
        let numChildren = D.numChildren rs
        numChildren `shouldEqual` 1

      it "can tell us a child does not exist" do
       	rs <- entriesSnapshot
        let noChild = D.hasChild rs "doesnotexist"
        noChild `shouldEqual` false

      it "can tell us a child exists" do
        rs <- entriesSnapshot
        let childExists = D.hasChild rs "-K7GbWeFHfJXlun7szRe" -- type Key = String ?
        childExists `shouldEqual` true

      it "can tell us the location at the snapshot exists" do
        sn <- entriesSnapshot
        (D.exists sn) `shouldEqual` true

      it "can tell us it has children" do
        sn <- entriesSnapshot
        let hasChildren = D.hasChildren sn
        hasChildren `shouldEqual` true

      pending "says the key of the database root is Nothing" {-
        -- Root has become inacessible due to permission tests :-( not sure how to test Nothing for key now
        do
        rs <- rootSnapshot
        let key = D.key rs
        key `shouldEqual` Nothing
      -}
      it "says the key of /entries is entries" do
        sn <- entriesSnapshot
        let key = D.key sn
        key `shouldEqual` (Just "entries")

      pending "it can not tell us the location at the snapshot does not exist"
        -- perhaps it can now, it might just have been an error callback with an additional () missing.
      pending "can it say the value of child \"entries\" is Nothing?"
        -- this relies on trying to read a firebase ref with once, and that 'works' by never being called back
      pending "it can give us a snapshot of one of its children"
      -- implement forEach callback that returns true after first call, so it terminates
      -- put that callback into Aff, so we can wait for the result
      -- with Aff, and the Aff variable, we can retrieve a list.
