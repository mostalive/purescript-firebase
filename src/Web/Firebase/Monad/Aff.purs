module Web.Firebase.Monad.Aff where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff.Exception (Error())

import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FB

foreign import fb2error :: FB.FirebaseErr -> Error

-- TODO this works for value, but will ignore the prevChild argument for onChildAdded etc.
on :: forall eff.
      FB.EventType ->
      FB.Firebase ->
      Aff (firebase :: FB.FirebaseEff | eff) FB.DataSnapshot
on etype fb = makeAff (\eb cb -> FB.on etype cb (Just $ onErr eb) fb)
  where
    -- convert firebase error to purescript Error in javascript
    -- see .js file for firebase Error documentation
    onErr :: (Error -> Eff (firebase :: FB.FirebaseEff | eff) Unit) ->
             FB.FirebaseErr ->
             Eff (firebase :: Web.Firebase.Types.FirebaseEff | eff) Unit
    onErr errorCallback firebaseError = errorCallback (fb2error firebaseError)
