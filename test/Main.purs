module Test.Main where

import Web.Firebase.Types as FBT
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Show (show)
import FirebaseTestConfig (firebaseTestDatabase)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Prelude (Unit, bind, discard, ($), (<$>))
import Test.Authentication (authenticationSpec)
import Test.Authorization (authorizationSpec)
import Test.DataSnapshotSpec (dataSnapshotSpec)
import Test.PromisesSpike (promisesSpikeSpec)
import Test.RefSpec (refSpec)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Web.Firebase (auth, initializeApp, database, rootRefFor)
import Web.Firebase.AdminSDK (AdminSDK, adminSDK, createCustomToken, mkCredentialCert, mkDatabaseName, mkUserId, everythingInJs, initializeApp) as Admin
import Web.Firebase.AdminSDK (CustomToken)
import Web.Firebase.Authentication.Aff (authWithCustomToken)
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types (Firebase, FirebaseEff, App)


type FbSpecEffects e =
  ( fs :: FS
  , err :: EXCEPTION
  , exception :: EXCEPTION
  , firebase :: FirebaseEff
  | e)
type FbSpecRunnerEffects e = RunnerEffects (FbSpecEffects e)

main ::  forall eff. Eff (FbSpecRunnerEffects eff) Unit
main = do
  root <- firebaseTestDatabase
  run [consoleReporter] (allSpecs root )

--allSpecs :: forall eff. StateT (Array (Group (Aff (FbSpecEffects eff) Unit))) Identity Unit
allSpecs :: forall eff. Firebase -> Spec ( FbSpecRunnerEffects eff ) Unit
allSpecs ref = do
  refSpec ref
  -- authorizationSpec ref
  -- authenticationSpec app auth token
  promisesSpikeSpec
  -- dataSnapshotSpec ref


readAdminFile =
  Admin.mkCredentialCert <$> readTextFile UTF8 "purescript-spike-firebase-adminsdk-d11ou-52e3eedfd8.json"
