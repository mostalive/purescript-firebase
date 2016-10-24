module Test.DataSnapshotSpec (dataSnapshotSpec) where

import Prelude (Unit, bind, ($), (>>=), (>=))

import Node.Process (PROCESS)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad (unless)
import Data.Maybe (Maybe(Just))
import Web.Firebase as FB
import Web.Firebase.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (fail, shouldEqual)

eSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
eSnapshot = snapshotFor "entries"

doesNotExist :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
doesNotExist = snapshotFor "entries/doesnotexist"

snapshotFor :: forall eff. String -> Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
snapshotFor location  = getRoot >>= \r -> (liftEff $ FB.child location r) >>= onceValue

getRoot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
getRoot = refFor "https://purescript-spike.firebaseio.com/"

expect :: forall r. Boolean -> Aff r Unit
expect condition = unless condition $ fail "false ≠ true"

dataSnapshotSpec ::  forall eff. FBT.DataSnapshot -> Spec ( process :: PROCESS, firebase :: FBT.FirebaseEff | eff) Unit
dataSnapshotSpec snapshot =
    describe "DataSnapshot" do
      -- literal API
      -- the difference between snapshots and refs is somewhat confusing
      it "can tell us the number of children" do
        let numChildren = D.numChildren snapshot
        expect (numChildren >= 1)

      it "can tell us a child does not exist" do
        (D.hasChild snapshot "doesnotexist")  `shouldEqual` false
      it "can tell us the location at the snapshot exists" do
        expect $ (D.exists snapshot)

      it "can tell us it has children" do
        expect $ D.hasChildren snapshot

      it "says the key of /entries is entries" do
        let key = D.key snapshot
        key `shouldEqual` (Just "entries")

      it "it can not tell us the location at the snapshot does not exist" do
        let noChild = (D.child snapshot "doesnotexist")
        (D.exists noChild) `shouldEqual` false
