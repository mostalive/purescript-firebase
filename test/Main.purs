module Test.Main where

import Prelude

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class (liftEff)
import Data.Either
import Data.Either.Unsafe  (fromRight)
import Data.URI (runParseURI)
import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FBT
import Test.Spec                  (describe, it)
import Test.Spec.Runner           (Process(), run)
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import PlayWithFire (readSuccessAff, Success(Success))

getRoot :: forall eff. Eff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
getRoot = do
  let fbUri = fromRight $ runParseURI "https://purescript-spike.firebaseio.com/"
  fb <- FB.newFirebase fbUri
  FB.child "entries" fb

getYes :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
getYes = do
  root <- liftEff $ getRoot
  readSuccessAff root

main ::  forall eff. Eff ( console :: CONSOLE, process :: Process, firebase :: FBT.FirebaseEff | eff) Unit
main = run [consoleReporter] do
  describe "Firebase" do
    describe "Reading once" do
      it "Child Added" do
        success <- getYes
        liftEff $ print success
        success `shouldEqual` (Right (Success {success: "Yes!"}))
