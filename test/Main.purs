module Test.Main where

import Effect (Effect)
import Effect.Aff (launchAff_)
import FirebaseTestConfig (firebaseConfig)
import Prelude (Unit, bind, discard, ($))
import Test.Authentication (authenticationSpec)
import Test.Authorization (authorizationSpec)
import Test.DataSnapshotSpec (dataSnapshotSpec)
import Test.RefSpec (refSpec)
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Web.Firebase (auth, initializeApp, database, rootRefFor)
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types as FBT

root :: String
root =  "https://purescript-spike.firebaseio.com/"

main ::  Effect Unit
main = do
  app <- initializeApp firebaseConfig
  db <- database app
  ref <- rootRefFor db
  a <- auth app
  runTests a ref

runTests ::  Auth -> FBT.Firebase  -> Effect Unit
runTests auth ref = launchAff_ $ runSpec [consoleReporter] (allSpecs auth ref)

--allSpecs :: StateT (Array (Group (Aff (FbSpecEffects eff) Unit))) Identity Unit
allSpecs :: Auth -> FBT.Firebase -> Spec Unit
allSpecs  auth ref = do
  refSpec ref
  authorizationSpec ref
  authenticationSpec auth
  dataSnapshotSpec ref
