module Test.DataSnapshotSpec (dataSnapshotSpec) where

import Control.Monad (unless)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Prelude (Unit, bind, discard, pure, ($), (<>), (>=), (>>=))
import Test.Spec (Spec, before, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Web.Firebase as FB
import Web.Firebase.Aff (child, onceValue, set) as DBA
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Testing (DatabaseName(..), adminApp)
import Web.Firebase.Types (DataSnapshot, DatabaseImpl, Firebase, Key)
import Web.Firebase.Types as FBT


dataSnapshotSpec ::  FBT.Firebase -> Spec Unit
dataSnapshotSpec _ =
  before addAnEntry  do
    describe "DataSnapshot" do
      it "can tell us the number of children" $ \snapshot -> do
        let numChildren = D.numChildren snapshot
        numChildren `shouldEqual` 1

      it "can tell us a child does not exist" $ \snapshot -> do
        (D.hasChild snapshot "doesnotexist")  `shouldEqual` false
      it "can tell us the location at the snapshot exists" $ \snapshot -> do
        expect $ (D.exists snapshot)

      it "can tell us it has children" $ \snapshot -> do
        expect $ D.hasChildren snapshot

      it "reads the name of the key" $ \snapshot -> do
        let key = D.key snapshot
        key `shouldEqual` (Just aPath)

      it "it can not tell us the location at the snapshot does not exist"$ \snapshot ->  do
        let noChild = (D.child snapshot "doesnotexist")
        (D.exists noChild) `shouldEqual` false

addAnEntry :: Aff DataSnapshot
addAnEntry = do
  app <- adminApp (DatabaseName "DataSnapshotApp")
  db <- liftEffect $ FB.database app
  ref <- liftEffect $ FB.rootRefFor db
  pathRef <- DBA.child aPath ref
  childRef <- DBA.child "firstchild"  pathRef
  DBA.set (unsafeToForeign "something") childRef
  DBA.onceValue pathRef

aPath :: String
aPath = "data_snapshot_spec_entries"

snapshotFor :: Firebase -> Key -> Aff FBT.DataSnapshot
snapshotFor ref location  = (liftEffect $ FB.child location ref) >>= DBA.onceValue

expect :: Boolean -> Aff Unit
expect condition = unless condition $ fail "false ≠ true"
