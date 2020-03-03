module Test.DataSnapshotSpec (dataSnapshotSpec) where

import Control.Monad (unless)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Prelude (Unit, bind, discard, ($), (<>), (>>=), (>=))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Web.Firebase as FB
import Web.Firebase.Aff (child, onceValue, set) as DBA
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types (DatabaseImpl, Firebase, Key)
import Web.Firebase.Types as FBT


dataSnapshotSpec ::  FBT.Firebase -> Spec Unit
dataSnapshotSpec ref =
  before (addAnEntry ref) do
    describe "DataSnapshot" do
      it "can tell us the number of children" do
        snapshot <- snapshotFor ref aPath
        let numChildren = D.numChildren snapshot
        expect (numChildren >= 1)

      it "can tell us a child does not exist" do
        snapshot <- snapshotFor ref aPath
        (D.hasChild snapshot "doesnotexist")  `shouldEqual` false
      it "can tell us the location at the snapshot exists" do
        snapshot <- snapshotFor ref aPath
        expect $ (D.exists snapshot)

      it "can tell us it has children" do
        snapshot <- snapshotFor ref aPath
        expect $ D.hasChildren snapshot

      it "says the key of /entries is entries" do
        snapshot <- snapshotFor ref aPath
        let key = D.key snapshot
        key `shouldEqual` (Just aPath)

      it "it can not tell us the location at the snapshot does not exist" do
        snapshot <- snapshotFor ref aPath
        let noChild = (D.child snapshot "doesnotexist")
        (D.exists noChild) `shouldEqual` false

addAnEntry :: DatabaseImpl -> Aff Unit
addAnEntry ref = do
  pathRef <- DBA.child (aPath <> "/firstchild") ref
  DBA.set (unsafeToForeign "something") pathRef

aPath :: String
aPath = "data_snapshot_spec_entries"

snapshotFor :: Firebase -> Key -> Aff FBT.DataSnapshot
snapshotFor ref location  = (liftEffect $ FB.child location ref) >>= DBA.onceValue

expect :: Boolean -> Aff Unit
expect condition = unless condition $ fail "false ≠ true"
