module Web.Firebase.Aff
-- | Firebase API translated to AFF
-- | mostly functions from standard API
-- | convenience functions not part of the api can be found in
-- | Web.Firebase.Aff.Read
(
  child
, convertError
, key
, offLocation
, on
, once
, onceValue
, push
, set
, fb2error
, firebaseErrToString
, toString
, remove
)
where

import Prelude (Unit, pure, ($), (<<<))

import Data.Foreign (Foreign, toForeign)
import Data.Nullable (toNullable)
import Data.Maybe (Maybe(Just,Nothing))
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Web.Firebase as FB
import Web.Firebase.Types as FBT

-- | Inspired by its Eff relative.
-- Throw takes a message and throws a MonadError in Aff with that message
throw :: forall eff a. String -> Aff eff a
throw = throwError <<< error

foreign import fb2error :: FBT.FirebaseErr -> Error
foreign import firebaseErrToString :: FBT.FirebaseErr -> String

-- | Gets a Firebase reference for the location at the specified relative path.
-- https://www.firebase.com/docs/web/api/firebase/child.html

child :: forall eff.
       FBT.Key ->
       FBT.Firebase ->
       Aff (firebase :: FBT.FirebaseEff | eff) FBT.Firebase
child aKey ref = liftEff $ FB.child aKey ref

-- | Returns the key of the current firebase reference
-- throws a MonadError if there was no key (i.e. when you ask for the key of the root reference, according to
-- https://www.firebase.com/docs/web/api/firebase/key.html
-- We made it an error, because asking a key of the root reference is a programming error, and should normally not happen.
-- One could specialize this in a Firebase type that can't be the root, or return '/' as the key.
key :: forall eff.
       FBT.Firebase ->
       Aff (firebase :: FBT.FirebaseEff | eff) FBT.Key
key fb = do
  let mKey = FB.key fb
  case mKey of
       Nothing -> throw "Key was null. Did you ask key of root reference?"
       Just k -> pure k

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
         Eff (firebase :: FBT.FirebaseEff | eff) Unit
convertError errorCallback firebaseError = errorCallback (fb2error firebaseError)

-- We also take the liberty to write more specific functions, e.g. once and on() in firebase have 4 event types. we get better error messages and code completion by making specific functions, e.g.
-- onvalue and onchildadded instead of on(value) and on(childAdded)

once :: forall e. FB.EventType -> FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
once eventType root = makeAff (\errorCb successCb ->
		                FB.once eventType successCb (convertError errorCb) root)

-- | write a value under a new generated key to the database
-- returns the firebase reference generated
push :: forall e. Foreign -> FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.Firebase
push value ref = makeAff (\onError onSuccess -> FB.pushA value onSuccess (convertError onError) ref)

set :: forall e. Foreign -> FBT.Firebase ->  Aff (firebase :: FBT.FirebaseEff | e) Unit
set value ref = makeAff (\onError onSuccess -> FB.setA value onSuccess (convertError onError) ref)

-- | Extra functions not part of firebase api, grown out of our use
offLocation :: forall e. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) Unit
offLocation = liftEff <<< FB.offSimple

onceValue :: forall e. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
onceValue root = once FB.Value root

-- | Get the absolute URL for this location -  https://firebase.google.com/docs/reference/js/firebase.database.Reference#toString

toString :: forall eff. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) String
toString = liftEff <<< FB.toString

-- | remove data below ref
-- (firebase will also remove the path to ref probably)
-- not a separate function on the API, but 'set null' which is not pretty in purescript
-- nor easy to understand
remove :: forall e. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) Unit
remove ref = set foreignNull ref
             where foreignNull = toForeign $ toNullable $ Nothing
