module Test.RefSpec (refSpec) where

import Web.Firebase.Aff as FAff
import Web.Firebase.Types as FBT
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (Unit, bind, discard, ($), (>>=), (<>))
import Test.Spec (pending, describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase (child, ref, rootRefFor, key, toString)

refSpec :: forall eff. FBT.Database -> FBT.Reference -> Spec (firebase :: FBT.FirebaseEff | eff) Unit
refSpec root rootRef = do
 describe "A ref in" do
   describe "Eff" do
     it "root key is Nothing" do
       let rootKey = key rootRef
       rootKey `shouldEqual` Nothing
     it "key of root refences' ref is ref" do
       actual <- liftEff $ (ref "achild" root)
       (key actual) `shouldEqual` (Just "achild")
     describe "toString" do
       it "on root represents full database URl" do
         actual <- liftEff $ (rootRefFor root) >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on / represents full database URl" do
         actual <- liftEff $ (ref "/" root) >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on ref represents full database URl plus ref" do
         actual <- liftEff $ (ref "achild" root) >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild"
       it "on child of ref represents full database URl plus child plus ref" do
         actual <- liftEff $
                   (ref "achild" root) >>= (child "second") >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild/second"
   describe "Aff" do
     describe "Getting a key" do
       it "on root throws an error" do
         rootRef <- liftEff $ rootRefFor root
         expectError $ FAff.key rootRef
       it "on ref of root returns ref" do
         actual <- (FAff.ref "affchild" root) >>= FAff.key
         actual `shouldEqual` "affchild"
     describe "toString" do
       it "on / returns database url" do
         actual <- FAff.ref "/" root >>= FAff.toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on root returns database url" do
         actual <- FAff.ref "/" root >>= FAff.toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on ref of root returns root url + ref" do
         actual <- FAff.ref "achild" root >>= FAff.toString
         actual `shouldEqual` ("https://purescript-spike.firebaseio.com/" <> "achild")
       it "on child of ref represents full database URl plus child plus ref" do
         actual <- (FAff.ref "achild" root) >>=
                   (FAff.child "second") >>=
                   FAff.toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild/second"
