module Test.ReadSpec where

import Prelude (Unit, bind, ($))

import Control.Apply ((*>))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (EXCEPTION(), message)
import Control.Alt ((<|>))
import Data.Either (either)
import Data.Foreign (toForeign)
import Web.Firebase.Types as FBT
import Web.Firebase (EventType(ChildMoved, ChildChanged, ChildRemoved, ChildAdded))
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.Monad.Aff (child, readOnceWithDefault)

-- | Reading from references and snapshots, sometimes with defaults
-- reading, as in also converting from foreign with some form of `read`
readSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff, err :: EXCEPTION | eff ) Unit
readSpec entries = do
    describe "Reading records and newtypes" do
        describe "with a default value" do
          it "returns default value on non-existant, but allowed location" do
            nodataLocation <- child "/nodata" entries
            nodata <- readOnceWithDefault "no data" nodataLocation
            nodata `shouldEqual` "no data"
