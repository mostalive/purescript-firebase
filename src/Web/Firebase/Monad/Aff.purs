module Web.Firebase.Monad.Aff
(
on,
once,
onceValue,
fb2error,
firebaseErrToString
)
where

import Prelude (Unit)
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff.Exception (Error())

import Web.Firebase as FB
import Web.Firebase.Types as FBT

foreign import fb2error :: FBT.FirebaseErr -> Error
foreign import firebaseErrToString :: FBT.FirebaseErr -> String

-- | This is the start of a more 'purescript-ish' interface than can be found in Web.Firebase
-- We use the Aff monad to eliminate callback hell
-- This way we can deal with callbacks that are called once.
-- We envision Web.Firebase.Signals to generate signals from callbacks that can be called multiple times

-- TODO this works for value, but will ignore the prevChild argument for onChildAdded etc.
on :: forall eff.
      FB.EventType ->
      FBT.Firebase ->
      Aff (firebase :: FBT.FirebaseEff | eff) FBT.DataSnapshot
on etype fb = makeAff (\eb cb -> FB.on etype cb (convertError eb) fb)

-- convert firebase error to purescript Error in javascript
-- see .js file for firebase Error documentation
convertError :: forall eff. (Error -> Eff (firebase :: FBT.FirebaseEff | eff) Unit) ->
	 FBT.FirebaseErr ->
         Eff (firebase :: Web.Firebase.Types.FirebaseEff | eff) Unit
convertError errorCallback firebaseError = errorCallback (fb2error firebaseError)

-- We also take the liberty to write more specific functions, e.g. once and on() in firebase have 4 event types. we get better error messages and code completion by making specific functions, e.g.
-- onvalue and onchildadded instead of on(value) and on(childAdded)

once :: forall e. FB.EventType -> FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
once eventType root = makeAff (\errorCb successCb ->
		                FB.once eventType successCb (convertError errorCb) root)

onceValue :: forall e. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
onceValue root = once FB.Value root
