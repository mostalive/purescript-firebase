module Test.Main where

import Prelude (Unit, bind)

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION())
import Web.Firebase.Types (FirebaseEff())
import Test.Spec.Runner           (Process(), run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Authorization (authorizationSpec)
import Test.Misc (miscSpec)
import Test.DataSnapshotSpec (dataSnapshotSpec)
import Test.WritingSpec (writingSpec)

main ::  forall eff. Eff ( console :: CONSOLE, err :: EXCEPTION, process :: Process, avar :: AVAR, firebase :: FirebaseEff | eff) Unit
main = run [consoleReporter] do
  authorizationSpec
  dataSnapshotSpec
  writingSpec
  miscSpec
