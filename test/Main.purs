module Test.Main where

import Prelude (Unit, bind, ($), (>>=))

import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Right))
import Web.Firebase as FB
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, pending, it)
import Test.Spec.Runner           (Process(), run)
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import PlayWithFire (readSuccessAff, Success(Success), snapshot2success)
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
import Data.Foreign as F

import Test.Authorization (authorizationSpec)
import Test.Misc (miscSpec)

main ::  forall eff. Eff ( console :: CONSOLE, process :: Process, firebase :: FBT.FirebaseEff | eff) Unit
main = run [consoleReporter] do
  authorizationSpec
  -- miscSpec
