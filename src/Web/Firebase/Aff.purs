module Web.Firebase.Aff
-- | Firebase API translated to AFF
-- | mostly functions from standard API
-- | convenience functions not part of the api can be found in
-- | Web.Firebase.Aff.Read
(
  child
, convertError
, database
, key
, offLocation
, on
, once
, onceValue
, push
, rootRefFor
, set
, fb2error
, firebaseErrToString
, toString
, remove
)
where

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Foreign (Foreign, unsafeToForeign)
import Prelude (Unit, pure, ($), (*>), (<<<), (>>>))
import Web.Firebase (EventType(..), child, database, key, offSimple, on, once, pushA,
                     rootRefFor, setA, toString) as FBE
import Web.Firebase.Types (DatabaseImpl, DataSnapshot, FirebaseErr, Key, Firebase, FirebaseAppImpl) as FBT

-- | Inspired by its Eff relative.
-- Throw takes a message and throws a MonadError in Aff with that message
throw :: forall a. String -> Aff a
throw = throwError <<< error

foreign import fb2error :: FBT.FirebaseErr -> Error
foreign import firebaseErrToString :: FBT.FirebaseErr -> String

-- | Gets a Firebase reference for the location at the specified relative path.
-- https://www.firebase.com/docs/web/api/firebase/child.html

child ::
       FBT.Key ->
       FBT.Firebase ->
       Aff FBT.Firebase
child aKey ref = liftEffect $ FBE.child aKey ref

-- | Returns the key of the current firebase reference
-- throws a MonadError if there was no key (i.e. when you ask for the key of the root reference, according to
-- https://www.firebase.com/docs/web/api/firebase/key.html
-- We made it an error, because asking a key of the root reference is a programming error, and should normally not happen.
-- One could specialize this in a Firebase type that can't be the root, or return '/' as the key.
key ::
       FBT.Firebase ->
       Aff FBT.Key
key fb = do
  let mKey = FBE.key fb
  case mKey of
       Nothing -> throw "Key was null. Did you ask key of root reference?"
       Just k -> pure k

-- | This is the start of a more 'purescript-ish' interface than can be found in Web.Firebase
-- We use the Aff monad to eliminate callback hell
-- This way we can deal with callbacks that are called once.
-- We envision Web.Firebase.Signals to generate signals from callbacks that can be called multiple times

-- TODO this works for value, but will ignore the prevChild argument for onChildAdded etc.
on :: FBE.EventType ->
      FBT.Firebase ->
      Aff FBT.DataSnapshot
on etype fb = makeAff (\cb -> FBE.on etype (Right >>> cb) (convertError (Left >>> cb)) fb *> pure nonCanceler)

-- convert firebase error to purescript Error in javascript
-- see .js file for firebase Error documentation
convertError :: (Error -> Effect Unit) ->
	 FBT.FirebaseErr ->
         Effect Unit
convertError errorCallback firebaseError = errorCallback (fb2error firebaseError)

-- We also take the liberty to write more specific functions, e.g. once and on() in firebase have 4 event types. we get better error messages and code completion by making specific functions, e.g.
-- onvalue and onchildadded instead of on(value) and on(childAdded)

once :: FBE.EventType -> FBT.Firebase -> Aff FBT.DataSnapshot
once eventType root = makeAff (\cb ->
  FBE.once eventType (Right >>> cb) (convertError (Left >>> cb)) root *> pure nonCanceler)

-- | write a value under a new generated key to the database
-- returns the firebase reference generated
push :: Foreign -> FBT.Firebase -> Aff FBT.Firebase
push value ref = makeAff (\cb -> FBE.pushA value (Right >>> cb) (convertError (Left >>> cb)) ref *> pure nonCanceler)

set :: Foreign -> FBT.Firebase ->  Aff Unit
set value ref = makeAff (\cb -> FBE.setA value (Right >>> cb) (convertError (Left >>> cb)) ref *> pure nonCanceler)

-- | Extra functions not part of firebase api, grown out of our use
offLocation :: FBT.Firebase -> Aff Unit
offLocation = liftEffect <<< FBE.offSimple

onceValue :: FBT.Firebase -> Aff FBT.DataSnapshot
onceValue root = once FBE.Value root

-- | Get the absolute URL for this location -  https://firebase.google.com/docs/reference/js/firebase.database.Reference#toString

toString :: FBT.Firebase -> Aff String
toString = liftEffect <<< FBE.toString

-- | remove data below ref
-- (firebase will also remove the path to ref probably)
-- not a separate function on the API, but 'set null' which is not pretty in purescript
-- nor easy to understand
remove :: FBT.Firebase -> Aff Unit
remove ref = set foreignNull ref
             where foreignNull = unsafeToForeign $ toNullable $ Nothing

database :: forall eff. MonadEffect eff => FBT.FirebaseAppImpl -> eff FBT.DatabaseImpl
database = liftEffect <<< FBE.database

rootRefFor :: forall eff. MonadEffect eff => FBT.DatabaseImpl -> eff FBT.DatabaseImpl
rootRefFor = liftEffect <<< FBE.rootRefFor
