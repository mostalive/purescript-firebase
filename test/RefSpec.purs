module Test.RefSpec (refSpec) where

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (Unit, bind, discard, ($), (>>=))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase (child, key, rootRefFor, toString)
import Web.Firebase.Aff as FAff
import Web.Firebase.Types as FBT
import Web.Firebase.UnsafeRef (refFor)

refSpec :: forall eff. FBT.FirebaseConfig -> Spec (firebase :: FBT.FirebaseEff | eff) Unit
refSpec config = do
  it "root reference is Nothing" do
    r <- liftEff $ rootRefFor config
    let rootKey = key r
    rootKey `shouldEqual` Nothing -- characterization test, we don't know, but we want to start somewhere
    
refSpecOld :: forall eff. String -> Spec (firebase :: FBT.FirebaseEff | eff ) Unit
refSpecOld root = do
    describe "a Database reference" do
      describe "key with Eff" do
       it "on root returns Nothing" do
         r <- refFor root
         let actual = key r
         actual `shouldEqual` Nothing -- see implementation of DataSnapshot key.js
       it "on child of root returns child" do
         r <- refFor root
         actual <- liftEff $ (child "achild" r) 
         (key actual) `shouldEqual` (Just "achild")
      describe "getting url with Eff" do
       it "on root returns url" do
         r <- refFor root
         actual <- liftEff $ toString r
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on child of root returns root url + child" do
         r <- refFor root
         actual <- liftEff $ (child "achild" r) >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild"
      describe "with Aff" do
        describe "Getting a key" do
         it "on root throws an error" do
           r <- refFor root
           expectError $ FAff.key r
         it "on child of root returns child" do
           r <- refFor root
           actual <- (FAff.child "affchild" r) >>= FAff.key
           actual `shouldEqual` "affchild"
        describe "getting the url" do
          it "on root returns url" do
            r <- refFor root
            actual <- FAff.toString r
            actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
          it "on child of root returns root url + child" do
            r <- refFor root
            actual <- (FAff.child "achild" r) >>= FAff.toString
            actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild"

