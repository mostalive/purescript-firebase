module Test.WriteGenericSpec (writeGenericSpec) where

import Prelude (Unit, bind, ($), class Show, class Eq)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(Nothing))
import Data.Either (Either(Right))
import Web.Firebase.Aff (onceValue, push, set)
import Web.Firebase.UnsafeRef (refFor)
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import Data.Foreign.Generic  (Options, defaultOptions, toForeignGeneric, readGeneric)
import Data.Generic (class Generic, gShow, gEq)

entriesRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
entriesRef = refFor "https://purescript-spike.firebaseio.com/entries/generic"

setRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
setRef = refFor "https://purescript-spike.firebaseio.com/entries/wecanset/generic/paths"

jsonOptions :: Options
jsonOptions = defaultOptions {unwrapNewtypes = true}

data MyBoolean = Yes | No | Perhaps | DontKnowYet

derive instance genericMyBoolean :: Generic MyBoolean

instance showMyBoolean :: Show MyBoolean where
  show = gShow

instance eqMyBoolean :: Eq MyBoolean where
  eq = gEq


newtype MyInvitation = MyInvitation {invitee :: String
                                , willAttend :: MyBoolean }

derive instance genericMyInvitation :: Generic MyInvitation

instance showMyInvitation :: Show MyInvitation where
  show = gShow

instance eqMyInvitation :: Eq MyInvitation where
  eq = gEq


noShow :: MyInvitation
noShow = MyInvitation {invitee: "someone", willAttend: No}

dontKnow :: MyInvitation
dontKnow = MyInvitation {invitee: "mr bean", willAttend: DontKnowYet }

-- | We chose to test Eff functions indirectly through Aff -
-- Eff tests were as least as complicated as the implementation
-- see Authorization tests for errors when writing
-- tests are with generic - default toForeign only works for very simple objects (e.g. strings and ints as values, Abstract Data Types for instance.
-- Once you get to maps, you are on your own - manual marshalling - maps don't have a Generic instance.
writeGenericSpec ::  forall eff. Spec ( firebase :: FBT.FirebaseEff, err :: EXCEPTION | eff) Unit
writeGenericSpec = do
  describe "Writing with Generics" do
      it "can add an ADT to a list" do
        location <- entriesRef
        newChildRef <- push (toForeignGeneric jsonOptions Yes) location
        snap <- onceValue newChildRef
        (D.key snap) `shouldNotEqual` Nothing
        (readGeneric jsonOptions (D.val snap)) `shouldEqual` (Right Yes)

      it "can overwrite an existing ADT" do
        let secondValue = {success: "second value"}
        location <- entriesRef
        newChildRef <- push (toForeignGeneric jsonOptions Yes) location
        set (toForeignGeneric jsonOptions No) newChildRef
        snap <- onceValue newChildRef
        (readGeneric jsonOptions (D.val snap)) `shouldEqual` (Right No)
      it "can read what push writes in new location" do
          location <- entriesRef
          newLocation <- push (toForeignGeneric jsonOptions noShow) location
          snap <- onceValue newLocation
          (readGeneric jsonOptions (D.val snap)) `shouldEqual` (Right noShow)
      it "can read what set writes in new location " do
          location <- setRef
          set (toForeignGeneric jsonOptions dontKnow) location
          snap <- onceValue location
          (readGeneric jsonOptions (D.val snap)) `shouldEqual` (Right dontKnow)
