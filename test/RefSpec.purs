module Test.RefSpec (refSpec) where

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (Unit, bind, discard, ($), (>>=), (<>))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase (child, key, toString)
import Web.Firebase.Aff as FAff
import Web.Firebase.Aff.Database (ref, rootRefFor) as AffDb
import Web.Firebase.Eff.Database (ref, rootRefFor)
import Web.Firebase.Types as FBT

refSpec :: forall eff. FBT.Database -> FBT.Reference -> Spec (firebase :: FBT.FirebaseEff | eff) Unit
refSpec root rootRef = do
 let db = root
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
         rootRef' <- liftEff $ rootRefFor root
         expectError $ FAff.key rootRef'
       it "on ref of root returns ref" do
         actual <- (AffDb.ref "affchild" root) >>= FAff.key
         actual `shouldEqual` "affchild"
     describe "toString" do
       it "on / returns database url" do
         actual <- AffDb.ref "/" root >>= FAff.toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on root returns database url" do
         actual <- AffDb.rootRefFor db >>= FAff.toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on ref of root returns root url + ref" do
         actual <- AffDb.ref "achild" root >>= FAff.toString
         actual `shouldEqual` ("https://purescript-spike.firebaseio.com/" <> "achild")
       it "on child of ref represents full database URl plus child plus ref" do
         actual <- (AffDb.ref "achild" root) >>=
                   (FAff.child "second") >>=
                   FAff.toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild/second"
