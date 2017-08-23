module Test.DataSnapshotSpec (dataSnapshotSpec) where

import Prelude (Unit, bind, discard, ($), (>>=), (>=))

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


dataSnapshotSpec ::  forall eff. Spec ( firebase :: FBT.FirebaseEff | eff) Unit
dataSnapshotSpec =
    describe "DataSnapshot" do
      -- literal API
      -- the difference between snapshots and refs is somewhat confusing
      it "can tell us the number of children" do
        snapshot <- eSnapshot
        let numChildren = D.numChildren snapshot
        expect (numChildren >= 1)

      it "can tell us a child does not exist" do
        snapshot <- eSnapshot
        (D.hasChild snapshot "doesnotexist")  `shouldEqual` false
      it "can tell us the location at the snapshot exists" do
        snapshot <- eSnapshot
        expect $ (D.exists snapshot)

      it "can tell us it has children" do
        snapshot <- eSnapshot
        expect $ D.hasChildren snapshot

      it "says the key of /entries is entries" do
        snapshot <- eSnapshot
        let key = D.key snapshot
        key `shouldEqual` (Just "entries")

      it "it can not tell us the location at the snapshot does not exist" do
        snapshot <- eSnapshot
        let noChild = (D.child snapshot "doesnotexist")
        (D.exists noChild) `shouldEqual` false

eSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
eSnapshot = snapshotFor "entries"

snapshotFor :: forall eff. String -> Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
snapshotFor location  = rootRef >>= \r -> (liftEff $ FB.child location r) >>= onceValue

doesNotExist :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
doesNotExist = snapshotFor "entries/doesnotexist"

rootRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
rootRef = refFor "https://purescript-spike.firebaseio.com/"

expect :: forall r. Boolean -> Aff r Unit
expect condition = unless condition $ fail "false ≠ true"
