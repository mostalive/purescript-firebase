module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import FirebaseTestConfig (firebaseConfig)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Prelude (Unit, bind, discard, ($), (<$>), (=<<))
import Test.Authentication (authenticationSpec)
import Test.Authorization (authorizationSpec)
import Test.DataSnapshotSpec (dataSnapshotSpec)
import Test.RefSpec (refSpec)
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Web.Firebase (auth, initializeApp, database, rootRefFor)
import Web.Firebase.AdminSDK (AdminSDK, adminSDK, createCustomToken, mkCredentialCert, mkDatabaseName, mkUserId, initializeApp) as Admin
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types (FirebaseEff)
import Web.Firebase.Types as FBT

root :: String
root =  "https://purescript-spike.firebaseio.com/"

type FbSpecEffects e =
  (fs :: FS
  , err :: EXCEPTION
  , exception :: EXCEPTION
  , firebase :: FirebaseEff
  | e)
type FbSpecRunnerEffects e = RunnerEffects (FbSpecEffects e)

main ::  forall eff. Eff (FbSpecRunnerEffects eff) Unit
main = do
  adminCert <- readAdminFile
  adminApp <- Admin.initializeApp adminCert (Admin.mkDatabaseName "purescript-spike") Admin.adminSDK
  token <- Admin.createCustomToken (Admin.mkUserId "testuser" ) Admin.adminSDK
  app <- initializeApp firebaseConfig
  db <- database app
  ref <- rootRefFor db
  a <- auth app
  runTests a ref

runTests ::  forall eff. Auth -> FBT.Firebase  -> Eff (FbSpecRunnerEffects eff) Unit
runTests auth ref = do run [consoleReporter] (allSpecs auth ref)

--allSpecs :: forall eff. StateT (Array (Group (Aff (FbSpecEffects eff) Unit))) Identity Unit
allSpecs :: forall eff. Auth -> FBT.Firebase -> Spec ( FbSpecRunnerEffects eff ) Unit
allSpecs  auth ref = do
  -- refSpec ref
  authorizationSpec ref
  -- authenticationSpec auth
  -- dataSnapshotSpec ref

readAdminFile =
  Admin.mkCredentialCert <$> readTextFile UTF8 "purescript-spike-firebase-adminsdk-d11ou-52e3eedfd8.json"
