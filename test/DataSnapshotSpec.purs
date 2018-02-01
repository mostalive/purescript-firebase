module Test.DataSnapshotSpec (dataSnapshotSpec) where

import Control.Monad (unless)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just))
import Prelude (Unit, bind, discard, ($), (>>=), (>=))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (fail, shouldEqual)
import Web.Firebase as FB
import Web.Firebase.Aff (onceValue)
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types (Firebase, Key)
import Web.Firebase.Types as FBT

dataSnapshotSpec ::  forall eff. FBT.Firebase -> Spec ( firebase :: FBT.FirebaseEff | eff) Unit
dataSnapshotSpec ref =
    -- literal API
    describe "DataSnapshot" do
      it "can tell us the number of children" do
        snapshot <- snapshotFor ref "entries"
        let numChildren = D.numChildren snapshot
        expect (numChildren >= 1)

      it "can tell us a child does not exist" do
        snapshot <- snapshotFor ref "entries"
        (D.hasChild snapshot "doesnotexist")  `shouldEqual` false
      it "can tell us the location at the snapshot exists" do
        snapshot <- snapshotFor ref "entries"
        expect $ (D.exists snapshot)

      it "can tell us it has children" do
        snapshot <- snapshotFor ref "entries"
        expect $ D.hasChildren snapshot

      it "says the key of /entries is entries" do
        snapshot <- snapshotFor ref "entries"
        let key = D.key snapshot
        key `shouldEqual` (Just "entries")

      it "it can not tell us the location at the snapshot does not exist" do
        snapshot <- snapshotFor ref "entries"
        let noChild = (D.child snapshot "doesnotexist")
        (D.exists noChild) `shouldEqual` false

snapshotFor :: forall eff. Firebase -> Key -> Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
snapshotFor ref location  = (liftEff $ FB.child location ref) >>= onceValue

expect :: forall r. Boolean -> Aff r Unit
expect condition = unless condition $ fail "false ≠ true"
