module Test.ReadSpec where

import Prelude (Unit, bind, ($), (>>=))

import Control.Apply ((*>))
import Control.Monad.Trans (lift)
import Control.Alt ((<|>))
import Data.Foreign (toForeign)

import Web.Firebase.Types as FBT
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual)
import Web.Firebase.Aff (child, onceValue, set)
import Web.Firebase.Aff.Read (readOnceWithDefault, readSnapshotWithDefault)

-- | Reading from references and snapshots, sometimes with defaults
-- reading, as in also converting from foreign with some form of `read`
readSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff | eff ) Unit
readSpec entries = do
  (lift (child "/nodata" entries)) >>= readSpecNoData
  do
    location <- (lift (child "/somedata" entries))
    lift (set (toForeign "some data") location)
    readSpecSomeData location

readSpecSomeData :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff | eff ) Unit
readSpecSomeData somedataLocation = do
    describe "Reading records and newtypes once" do
        describe "with a default value" do
          it "returns default value on some-existant and allowed location" do
            somedata <- readOnceWithDefault "no data" somedataLocation
            somedata `shouldEqual` "some data"
    describe "Reading a snapshot" do
        describe "with a default value" do
          it "returns default value on some-existant and allowed location" do
            somedataSnapshot <- onceValue somedataLocation
            somedata <- readSnapshotWithDefault "no data" somedataSnapshot
            somedata `shouldEqual` "some data"

readSpecNoData :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff | eff ) Unit
readSpecNoData nodataLocation = do
    describe "Reading records and newtypes once" do
        describe "with a default value" do
          it "returns default value on non-existant, but allowed location" do
            nodata <- readOnceWithDefault "no data" nodataLocation
            nodata `shouldEqual` "no data"
    describe "Reading a snapshot" do
        describe "with a default value" do
          it "returns default value on non-existant, but allowed location" do
            nodataSnapshot <- onceValue nodataLocation
            nodata <- readSnapshotWithDefault "no snapshot data" nodataSnapshot
            nodata `shouldEqual` "no snapshot data"

