module Test.Main where

import Prelude (Unit, ($), bind, (>>=))

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Trans (lift)
import Node.Process (PROCESS)
import Web.Firebase.Types (FirebaseEff())
import Web.Firebase as FB
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
import Web.Firebase.Types as FBT
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec (Spec())

import Test.Authorization (authorizationSpec)
import Test.Authentication (authenticationSpec)
import Test.ReadSpec (readSpec)
import Test.RefSpec (refSpec)
import Test.DataSnapshotSpec (dataSnapshotSpec)
import Test.WriteGenericSpec (writeGenericSpec)
import Test.AuthDataSpec (authDataSpec)

eSnapshot :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
eSnapshot = snapshotFor "entries"

snapshotFor :: forall eff. String -> Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
snapshotFor location  = rootRef >>= \r -> (liftEff $ FB.child location r) >>= onceValue

-- | Determines the name of the database to use for testing
-- Change this when you want tests to run against another database
rootRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
rootRef = refFor "https://purescript-spike.firebaseio.com/"

-- | Reference to /entries that can be written to and read from
-- It is clearest if each test suite uses its own child path
-- so that after a test run data written can be easily inspected
-- if necessary
entriesRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
entriesRef = refFor "https://purescript-spike.firebaseio.com/entries"

forbiddenRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
forbiddenRef = refFor "https://purescript-spike.firebaseio.com/forbidden"

main ::  forall eff. Eff ( console :: CONSOLE, err :: EXCEPTION, process :: PROCESS, avar :: AVAR, firebase :: FirebaseEff | eff) Unit
main = run [consoleReporter] allSpecs

allSpecs :: forall eff. Spec (  console :: CONSOLE, err :: EXCEPTION, process :: PROCESS, avar :: AVAR, firebase :: FirebaseEff | eff) Unit
allSpecs = do
  ((lift forbiddenRef) >>= authorizationSpec)
  ((lift rootRef) >>= authenticationSpec)
  ((lift rootRef) >>= refSpec)
  ((lift entriesRef) >>= readSpec)
  ((lift eSnapshot) >>= dataSnapshotSpec)
  writeGenericSpec
  authDataSpec
  -- add link to twitterwall home screen 'login with twitter' styled as a twitter button.
  -- save userid under /users,
  -- only writeable to authenticated users
  -- add roles under /roles, only writeable by conf1r3
  -- add admin role to conf1r3
  -- and then roles only writeable by those with admin role, readable by authenticated users

