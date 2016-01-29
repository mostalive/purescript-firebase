module Test.Authorization where

import Prelude (Unit, bind, (>>=), ($), (<<<), pure)

import Control.Monad.Aff (Aff(),forkAff,later', runAff, launchAff)
import Control.Monad.Aff.Par (Par(..), runPar)
import Control.Monad.Aff.AVar (AVAR(), makeVar, takeVar, putVar)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Console (CONSOLE(), print)
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

        it "with handcrafted Aff throws an error" do
          snap <- forbiddenRef >>= onceValue -- catch error thrown and assert
          "should not" `shouldEqual` "get here"

        it "with Aff times out where it shold not" do
          respVar <- makeVar
          let  slow = (later' 400 $ putVar respVar "timed out")
               fot = do snap <- forbiddenRef >>= onceValue
                        let childexists = D.hasChild snap "fruit" -- type key = string ?
                        if childexists then (putVar respVar "forbidden ref found") else (putVar respVar "child not forbidden")
          handle <- forkAff fot
          slowhandle <- forkAff slow
          actual <- takeVar respVar
          actual `shouldEqual` "child forbidden"
      {-  it "with aff returns an error object" do
          let snap = forbiddenref >>= oncevalue
          snap `shouldequal` (Left "permission_denied")
          would we not expect an either to come back? how does an Aff signal that it had an error? throw?. Our code is not doing that, and beforehand error was not even evaluated.
      -}


      -- this forces us to handle errors in aff, and parse error objects
      -- documentation on firebase error was hard to find, having an actual one would allow us to write some marshalling code. code and message should be present, details wrapped in a maybe. given we don't own this interface, placing a console.log in the ffi javascript side is wise for now.


	 {- https://www.firebase.com/docs/web/guide/user-auth.html#section-handling-errors:
  All errors are Error objects containing at least code and message attributes. In some cases, additional information will be provided via the details attribute. For example:
  {
	    code: "TRANSPORT_UNAVAILABLE",
       message: "There are no login transports available for the requested method.",
         details: "More details about the specific error here."
 }
	-}
