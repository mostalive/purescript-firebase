module Test.Main where

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Trans.Class (lift)
import Data.Identity (Identity(..))
import Node.Process (PROCESS)
import Prelude (Unit, bind, ($), discard, (>>=))
import Test.Authentication (authenticationSpec)
import Test.Authorization (authorizationSpec)
import Test.DataSnapshotSpec (dataSnapshotSpec)
import Test.RefSpec (refSpec)
import Test.Spec (Group, Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Web.Firebase as FB
import Web.Firebase.Aff.Read (onceValue)
import Web.Firebase.Types (FirebaseEff)
import Web.Firebase.Types as FBT
import Web.Firebase.UnsafeRef (refFor, unsafeRef)

eSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
eSnapshot = snapshotFor "entries"

snapshotFor :: forall eff. String -> Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
snapshotFor location  = rootRef >>= \r -> (liftEff $ FB.child location r) >>= onceValue

-- | Determines the name of the database to use for testing
-- Change this when you want tests to run against another database
rootRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
rootRef = refFor "https://purescript-spike.firebaseio.com/"

root :: String
root =  "https://purescript-spike.firebaseio.com/"

-- | Reference to /entries that can be written to and read from
-- It is clearest if each test suite uses its own child path
-- so that after a test run data written can be easily inspected
-- if necessary
entriesRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
entriesRef = refFor "https://purescript-spike.firebaseio.com/entries"

forbiddenRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
forbiddenRef = refFor "https://purescript-spike.firebaseio.com/forbidden"
-- Console, timer, avar, process

type FbSpecEffects e = (err :: EXCEPTION, firebase :: FirebaseEff | e)
type FbSpecRunnerEffects e = RunnerEffects (FbSpecEffects e)

main ::  forall eff. Eff (FbSpecRunnerEffects eff) Unit
main = do
  run [consoleReporter] allSpecs

--allSpecs :: forall eff. StateT (Array (Group (Aff (FbSpecEffects eff) Unit))) Identity Unit
allSpecs :: forall t31.
  Spec ( firebase :: FBT.FirebaseEff
                 | t31
       ) Unit
allSpecs  = do
  authorizationSpec
  authenticationSpec
  refSpec root
  dataSnapshotSpec


