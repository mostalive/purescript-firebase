module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Prelude (Unit, bind, discard)
import Test.Authentication (authenticationSpec)
import Test.Authorization (authorizationSpec)
import Test.DataSnapshotSpec (dataSnapshotSpec)
import Test.RefSpec (refSpec)
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Web.Firebase (rootRefFor)
import Web.Firebase.Types (FirebaseEff)
import Web.Firebase.Types as FBT
import FirebaseTestConfig(firebaseConfig)

root :: String
root =  "https://purescript-spike.firebaseio.com/"

type FbSpecEffects e = (err :: EXCEPTION, firebase :: FirebaseEff | e)
type FbSpecRunnerEffects e = RunnerEffects (FbSpecEffects e)

main ::  forall eff. Eff (FbSpecRunnerEffects eff) Unit
main = do
  r <- rootRefFor firebaseConfig
  runTests r

runTests ::  forall eff. FBT.Firebase  -> Eff (FbSpecRunnerEffects eff) Unit
runTests ref = do run [consoleReporter] (allSpecs ref)

--allSpecs :: forall eff. StateT (Array (Group (Aff (FbSpecEffects eff) Unit))) Identity Unit
allSpecs :: forall eff. FBT.Firebase -> Spec ( firebase :: FBT.FirebaseEff | eff ) Unit
allSpecs  ref = do
  refSpec ref

setAsideForNow :: forall eff. Spec ( firebase :: FirebaseEff | eff  ) Unit
setAsideForNow = do
  authorizationSpec firebaseConfig
  authenticationSpec
  dataSnapshotSpec
