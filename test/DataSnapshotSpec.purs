module Test.DataSnapshotSpec (dataSnapshotSpec) where

import Control.Monad (unless)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Just))
import Prelude (Unit, bind, discard, pure, ($), (<$>), (<*>), (>=), (>>=))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (fail, shouldEqual)
import Web.Firebase.Aff (child, onceValue)
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types (Firebase, Key)
import Web.Firebase.Types as FBT

dataSnapshotSpec ::  forall eff. FBT.Firebase -> Spec ( firebase :: FBT.FirebaseEff | eff) Unit
dataSnapshotSpec ref = do
  let snap = snapshotFor ref "entries"
  describe "DataSnapshot" do
    it "can tell us the number of children" do
      snapshot <- snapshotFor ref "entries"
      numChildren <- D.numChildren <$> snap
      expect (numChildren >= 1)

    it "can tell us a child does not exist" do
      snapshot <- snapshotFor ref "entries"
      (D.hasChild snapshot "doesnotexist")  `shouldEqual` false
    it "can tell us the location at the snapshot exists" do
      (D.exists <$> snap) >>= expect
    it "can tell us it has children" do
      D.hasChildren <$> snap >>= expect
    it "says the key of /entries is entries" do
      key <- D.key <$> snap
      key `shouldEqual` (Just "entries")
    it "it can not tell us the location at the snapshot does not exist" do
      noChild <- (D.child <$> snap <*> pure "doesnotexist")
      (D.exists noChild) `shouldEqual` false

snapshotFor :: forall eff. Firebase -> Key -> Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
snapshotFor ref key  = (child key ref ) >>= onceValue

expect :: forall r. Boolean -> Aff r Unit
expect condition = unless condition $ fail "false ≠ true"
