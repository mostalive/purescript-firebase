module Test.RefSpec (refSpec) where

import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (Unit, bind, discard, ($), (>>=), (<>))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase (child, key, toString)
import Web.Firebase.Aff as FAff
import Web.Firebase.Types as FBT

refSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff | eff) Unit
refSpec ref = do
 describe "A reference in" do
   describe "Eff" do
     it "root reference is Nothing" do
       let rootKey = key ref
       rootKey `shouldEqual` Nothing
     it "key of root refences' child is child" do
       actual <- liftEff $ (child "achild" ref)
       (key actual) `shouldEqual` (Just "achild")
     describe "toString" do
       it "on root represents full database URl" do
         actual <- liftEff $ (child "/" ref) >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on child represents full database URl plus child" do
         actual <- liftEff $ (child "achild" ref) >>= toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/achild"
   describe "Aff" do
     describe "Getting a key" do
       it "on root throws an error" do
         expectError $ FAff.key ref
       it "on child of root returns child" do
         actual <- (FAff.child "affchild" ref) >>= FAff.key
         actual `shouldEqual` "affchild"
     describe "toString" do
       it "on root returns url" do
         actual <- FAff.child "/" ref >>= FAff.toString
         actual `shouldEqual` "https://purescript-spike.firebaseio.com/"
       it "on child of root returns root url + child" do
         let child = "achild"
         actual <- (FAff.child child ref) >>= FAff.toString
         actual `shouldEqual` ("https://purescript-spike.firebaseio.com/" <> child)
