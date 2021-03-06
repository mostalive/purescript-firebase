module Test.RefSpec (refSpec) where

import Control.Monad (class Monad)
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prelude (Unit, bind, discard, ($), (>>=), (<>))
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, expectError)
import Web.Firebase (child, key, toString)
import Web.Firebase.Aff as FAff
import Web.Firebase.Types (DatabaseImpl)

refSpec :: forall eff. Monad eff => DatabaseImpl -> SpecT Aff Unit eff Unit
refSpec ref = do
 describe "A reference in" do
   describe "Eff" do
     it "root reference is Nothing" do
       let rootKey = key ref
       rootKey `shouldEqual` Nothing
     it "key of root refences' child is child" do
       actual <- liftEffect $ (child "achild" ref)
       (key actual) `shouldEqual` (Just "achild")
     describe "toString" do
       it "on root represents full database URl" do
         actual <- liftEffect $ toString ref
         actual `shouldEqual` dbUrl
       it "on child represents full database URl plus child" do
         actual <- liftEffect $ (child "achild" ref) >>= toString
         actual `shouldEqual` (dbUrl <> "achild")
   describe "Aff" do
     describe "Getting a key" do
       it "on root throws an error" do
         expectError $ FAff.key ref
       it "on child of root returns child" do
         actual <- (FAff.child "affchild" ref) >>= FAff.key
         actual `shouldEqual` "affchild"
     describe "toString" do
       it "on root returns url" do
         actual <- FAff.toString ref
         actual `shouldEqual` dbUrl
       it "on child of root returns root url + child" do
         let child = "achild"
         actual <- (FAff.child child ref) >>= FAff.toString
         actual `shouldEqual` (dbUrl <> child)

dbUrl :: String
dbUrl = "http://localhost:9000/"
