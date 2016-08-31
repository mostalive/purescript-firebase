module Test.RefSpec (refSpec) where

import Prelude (Unit, bind, ($), (>>=))

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Maybe (Maybe(Just, Nothing))
import Web.Firebase.Types as FBT
import Web.Firebase (child, key, toString)
import Web.Firebase.Aff as FAff
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)

refSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff, err :: EXCEPTION, console :: CONSOLE | eff ) Unit
refSpec rootRef = do
    describe "a Database reference" do
      describe "key with Eff" do
       it "on root returns Nothing" do
         actual <- liftEff $ key rootRef
         actual `shouldEqual` Nothing -- see implementation of DataSnapshot key.js
       it "on child of root returns child" do
         actual <- liftEff $ (child "achild" rootRef) >>= key
         actual `shouldEqual` (Just "achild")
      describe "getting url with Eff" do
       it "on root returns url" do
         actual <- liftEff $ toString rootRef
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on child of root returns root url + child" do
         actual <- liftEff $ (child "achild" rootRef) >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild"
      describe "with Aff" do
        describe "Getting a key" do
         it "on root throws an error" do
           expectError $ FAff.key rootRef
         it "on child of root returns child" do
           actual <- (FAff.child "affchild" rootRef) >>= FAff.key
           actual `shouldEqual` "affchild"
        describe "getting the url" do
          it "on root returns url" do
            actual <- FAff.toString rootRef
            actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
          it "on child of root returns root url + child" do
            actual <- (FAff.child "achild" rootRef) >>= FAff.toString
            actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild"

