module Test.Authorization where

import Prelude (Unit, bind, (>>=), ($), (<<<), pure)

import Control.Monad.Aff (Aff(),forkAff,later', runAff, launchAff, attempt)
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Aff.AVar (AVAR(), makeVar, takeVar, putVar)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), message)
import Control.Monad.Eff.Console (CONSOLE(), print)
import Control.Monad.Error.Class (throwError)
import Control.Alt ((<|>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Web.Firebase (EventType(ChildAdded),once)
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
import Data.Foreign as F

forbiddenRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
forbiddenRef = refFor "https://purescript-spike.firebaseio.com/forbidden"

authorizationSpec :: forall eff. Spec (firebase :: FBT.FirebaseEff, err :: EXCEPTION, avar :: AVAR, console :: CONSOLE | eff ) Unit
authorizationSpec = do
    describe "Understanding AVar and Par" do
      it "can write to a var" do
        respVar <- makeVar
        handle <- forkAff (later' 100 $ putVar respVar true)
        actual <- takeVar respVar
        actual `shouldEqual` true
      it "can race two vars manually" do
        respVar <- makeVar
        handle <- forkAff (later' 100 $ putVar respVar "fast")
        handleSlow <- forkAff (later' 200 $ putVar respVar "slow")
        actual <- takeVar respVar
        actual `shouldEqual` "fast"
      it "can race two vars with an alternative" do
        let fast = (later' 100 $ pure "fast")
            slow = (later' 200 $ pure "slow")
        actual <- runPar (Par fast <|> Par slow)
        actual `shouldEqual` "fast"
    describe "Authorization" do
      describe "once() on forbidden location" do
        it "with Eff calls an error callback" do
          deref <- forbiddenRef
          respVar <- makeVar
          handle  <- liftEff $ once ChildAdded (\snap -> launchAff $ putVar respVar "unexpected sucess") (Just (\_ -> launchAff $ putVar respVar "child forbidden")) deref
          actual <- takeVar respVar
          actual `shouldEqual` "child forbidden"
          -- the above works. it feels like the types of once (Eff vesion) are wrong, makeAff suggests it keeps the input parameter of the success callback, and Takes an error and puts it into an effect
          -- that suggests there should be a 'throwError' or something when the effect callback is called. Also, the type of that callback is 'error'

        it "with Aff throws an error" do
           e <- attempt $ forbiddenRef >>= onceValue -- catch error thrown and assert
           either (\err -> (message err) `shouldEqual` "permission_denied: Client doesn't have permission to access the desired data.\n | firebase code: | \n PERMISSION_DENIED") (\_ -> "expected an error to be thrown" `shouldEqual` "but was not") e


	 {- see also Firebase.Types
  https://www.firebase.com/docs/web/guide/user-auth.html#section-handling-errors:
  All errors are Error objects containing at least code and message attributes. In some cases, additional information will be provided via the details attribute. For example:
  {
	    code: "TRANSPORT_UNAVAILABLE",
       message: "There are no login transports available for the requested method.",
         details: "More details about the specific error here."
 }
	-}
