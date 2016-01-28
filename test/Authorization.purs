module Test.Authorization where

import Prelude (Unit, bind, (>>=))

import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe(Just, Nothing))
import Web.Firebase.DataSnapshot as D
import Web.Firebase.Types as FBT
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual, shouldNotEqual)
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.UnsafeRef (refFor)
import Data.Foreign as F

forbiddenRef :: forall eff. Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
forbiddenRef = refFor "https://purescript-spike.firebaseio.com/forbidden"

authorizationSpec :: forall eff. Spec (firebase :: FBT.FirebaseEff | eff ) Unit
authorizationSpec = do
    describe "Authorization" do
      it "returns an error object subscribing to an unauthorized location" do
        snap <- forbiddenRef >>= onceValue
        let childExists = D.hasChild snap "fruit" -- type Key = String ?
        (D.key snap) `shouldNotEqual` (Just "fruit")
        (D.key snap) `shouldEqual` Nothing
        childExists `shouldEqual` false
      -- this forces us to handle errors in Aff, and parse Error objects
      -- documentation on firebase Error was hard to find, having an actual one would allow us to write some marshalling code. code and message should be present, details wrapped in a Maybe. Given we don't own this interface, placing a console.log in the FFi javascript side is wise for now.


	 {- https://www.firebase.com/docs/web/guide/user-auth.html#section-handling-errors:
  All errors are Error objects containing at least code and message attributes. In some cases, additional information will be provided via the details attribute. For example:
  {
	    code: "TRANSPORT_UNAVAILABLE",
       message: "There are no login transports available for the requested method.",
         details: "More details about the specific error here."
 }
	-}
