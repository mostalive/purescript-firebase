module Web.Firebase
( EventType(..)
, auth
, child
, database
, initializeApp
, key
, newFirebase
, offSimple
, on
, once
, push
, pushA
, pushE
, rootRefFor
, set
, setE
, setA
, toString
)
where

import Effect (Effect)
import Foreign (Foreign)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, toNullable, Nullable)
import Prelude (class Show, class Eq, class Ord, Unit, (<$>), (<<<), compare, map)
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types (App, DataSnapshot, DatabaseImpl, FirebaseAppImpl, FirebaseConfig, FirebaseErr, Key, Firebase)
import Web.Firebase.Unsafe (unsafeEvalEff)

-- https://firebase.google.com/docs/reference/js/firebase.auth.Auth
auth :: FirebaseAppImpl -> Effect Auth
auth = runFn1 authImpl

foreign import newFirebaseImpl :: Fn1 String (Effect Firebase)
--foreign import databaseImpl :: forall eff. Fn1 FirebaseAppImpl (Eff (firebase :: FirebaseEff | eff) Firebase)
foreign import initializeAppImpl :: Fn1 FirebaseConfig (Effect FirebaseAppImpl)
foreign import databaseImpl :: Fn1 FirebaseAppImpl (Effect Firebase )

foreign import authImpl :: Fn1 FirebaseAppImpl (Effect Auth)
foreign import rootRefForImpl :: Fn1 DatabaseImpl (Effect Firebase)

rootRefFor :: DatabaseImpl -> Effect Firebase
rootRefFor = runFn1 rootRefForImpl

-- | Data.URI would introduce too many dependencies for this single use
-- | if you want URI's checked, import Data.URI in your projects, and use printURI to convert
-- | We assume an application sets up a newFirebase just once,
-- | and if the URI is wrong, that it is a programming error
type FirebaseURI = String

-- legacy
newFirebase :: FirebaseURI -> Effect Firebase
newFirebase u = runFn1 newFirebaseImpl u

initializeApp :: FirebaseConfig -> Effect App
initializeApp = runFn1 initializeAppImpl

database :: FirebaseAppImpl -> Effect DatabaseImpl
database = runFn1 databaseImpl


-- instance firebaseApp :: App FirebaseAppImpl where
--  database impl =

-- | Gets a Firebase reference for the location at the specified relative path.
-- https://www.firebase.com/docs/web/api/firebase/child.html
-- Firebase documentation does not specify what happens when the child does not exist
-- Probably nothing happens when the path does not exist, since references don't involve IO, they are just data about paths in the DB that
-- may or may not exist.
-- Existence can be checked by getting the value, and DataSnapshot.exists - https://www.firebase.com/docs/web/api/datasnapshot/exists.html

foreign import childImpl :: Fn2 String Firebase (Effect Firebase)

child :: String -> Firebase -> Effect Firebase
child = runFn2 childImpl

-- | Gets the key of the reference - https://www.firebase.com/docs/web/api/firebase/key.html
-- Nothing on the root ref (following the behaviour in the Firebase API)
foreign import _key :: Fn1 Firebase (Nullable Key)

key :: Firebase -> Maybe Key
key ds = map toMaybe _key ds

data EventType = Value
               | ChildAdded
               | ChildChanged
               | ChildRemoved
               | ChildMoved

-- | Boilerplate, but we have data instead of newtype
-- so generics don't work here
instance eqEventType :: Eq EventType where
  eq Value        Value = true
  eq ChildAdded   ChildAdded = true
  eq ChildChanged ChildChanged = true
  eq ChildRemoved ChildRemoved = true
  eq ChildMoved   ChildMoved = true
  eq _            _     = false

-- | Boilerplate, but we have data instead of newtype
-- so generics don't work here
-- provided so we can make a set of EventTypes
instance ordEventType :: Ord EventType where
  compare ev1 ev2 = compare (numValue ev1) (numValue ev2)
                where
                        numValue Value = 0
                        numValue ChildAdded = 1
                        numValue ChildChanged = 2
                        numValue ChildRemoved = 3
                        numValue ChildMoved = 4



showEventType :: EventType -> String
showEventType t = case t of
                       Value -> "value"
                       ChildAdded -> "child_added"
                       ChildChanged -> "child_changed"
                       ChildRemoved -> "child_removed"
                       ChildMoved -> "child_moved"

instance show4EventType :: Show EventType where
  show = showEventType

-- | Listens for data changes at a particular location
-- https://www.firebase.com/docs/web/api/query/on.html
-- Error callback in this FFI is not optional, see onWithoutCancelCallbackImpl why.
foreign import onImpl :: Fn4
                   String
                   (DataSnapshot -> Effect Unit)
                   (FirebaseErr -> Effect Unit)
                   Firebase
                   (Effect Unit)

on ::
      EventType ->
      (DataSnapshot -> Effect Unit) ->
      (FirebaseErr -> Effect Unit) ->
      Firebase ->
      Effect Unit
on etype ds canceler fb = runFn4 onImpl (showEventType etype) ds canceler fb

-- | this function is only present for documentation purposes
-- so you can see why we error callbacks are not optional in this FFI.
--- The documentation mentions the Cancel callback is optional.
-- It does not mention that your program will halt with a Javascript error.
-- When an error occurs while no callback is passed to on,  or when null is passed, it throws a javascript exception:
--  FIREBASE WARNING: Exception was thrown by user callback. TypeError: errorCallback is not a function.
foreign import onWithoutCancelCallbackImpl :: Fn3
                   String
                   (DataSnapshot -> Effect Unit)
                   Firebase
                   (Effect Unit)

-- | unsubscribes from a location
-- optionally for a specific callback or type. We don't need that in our application right now, so not going to bother with Maybes just now. just take the ref
-- and add the other parameters later.
-- https://www.firebase.com/docs/web/api/query/off.html
foreign import _offSimple :: Fn1
                Firebase
                (Effect Unit)


offSimple :: Firebase -> Effect Unit
offSimple ref = runFn1 _offSimple ref

-- implementation of off with more parameters goes here. we can then rewrite offSimple in terms of off.

-- | Listens for one change at a particular location
-- Takes a success and an error callback.
-- Error callback is not optional in this implementation, see onWithoutCancleCallbackImpl for why.
foreign import onceImpl :: Fn4
        String
        (DataSnapshot -> Effect Unit)
        (FirebaseErr -> Effect Unit)
        Firebase
        (Effect Unit)

once ::
        EventType ->
        (DataSnapshot -> Effect Unit) ->
        (FirebaseErr -> Effect Unit) ->
        Firebase ->
        Effect Unit
once etype ds cb fb = runFn4 onceImpl (showEventType etype) (unsafeEvalEff <<< ds) (cb) fb

-- | sets a value in the firebase https://www.firebase.com/docs/web/api/firebase/set.html
-- this implementation is also broken, FirebaseErr can be null. if you implement a callback it will fail
-- fixing setImplE first, purescript semantics make more sense: if you don't want to handle errors,
-- you can allways pass a callback that ignores its input: noop _ = pure Unit
-- Also, push hang when an error would occur and Nothing was passed. Not sure how that happens. This probably behaves similarly.
foreign import setImpl :: Fn3
                   Foreign
                   (Nullable (Nullable (FirebaseErr -> Effect Unit)))
                   Firebase
                   (Effect Unit)

set ::
       Foreign ->
       Maybe (Maybe (FirebaseErr -> Effect Unit)) ->
       Firebase ->
       Effect Unit
set value cb fb = runFn3 setImpl value (toNullable (toNullable <$> cb)) fb

-- | an extra implementation of set() that forces you to provide an error callback, even if it is a noop (see set in the example above)
-- FirebaseErr can be null, so we wrap it in a Maybe
foreign import setEImpl :: Fn3
                   Foreign
                   ((Nullable FirebaseErr) -> Effect Unit)
                   Firebase
                   (Effect Unit)

setE ::
       Foreign ->
       ((Maybe FirebaseErr) -> Effect Unit) ->
       Firebase ->
       Effect Unit
setE value cb fb = runFn3 setEImpl value (callBackReceivesNull cb) fb

-- | set with success and error callback, for easy Aff conversion with makeAff
-- explicit parameter is easier and communicates better than messing with Maybes and nullables.
foreign import _setA :: Fn4
                   Foreign
                   (Unit -> Effect Unit)
                   (FirebaseErr -> Effect Unit)
                   Firebase
                   (Effect Unit)

setA ::
        Foreign ->
        (Unit -> Effect Unit) ->
        (FirebaseErr -> Effect Unit) ->
        Firebase ->
        Effect Unit
setA = runFn4 _setA

-- | this one is broken. hangs with Nothing passed for callback (so undefined actually might not work)
-- also hangs when Just (Just callback) is passed, even when calling the effect.
foreign import pushImpl :: Fn3
                   Foreign
                   (Nullable (Nullable (FirebaseErr -> Effect Unit)))
                   Firebase
                   (Effect Firebase)

push ::
        Foreign ->
        Maybe (Maybe (FirebaseErr -> Effect Unit)) ->
        Firebase ->
        Effect Firebase
push value cb fb = runFn3 pushImpl value (toNullable (toNullable <$> cb)) fb


-- | push with FirebaseErr error callback
-- explicit parameter is easier and communicates better than messing with Maybes and nullables.
foreign import pushEImpl :: Fn3
                   Foreign
                   ((Nullable FirebaseErr) -> Effect Unit)
                   Firebase
                   (Effect Firebase)

pushE ::
        Foreign ->
        ((Maybe FirebaseErr) -> Effect Unit) ->
        Firebase ->
        Effect Firebase
pushE value cb fb = runFn3 pushEImpl value (callBackReceivesNull cb) fb


-- | push with success and error callback, for easy Aff conversion with makeAff
-- explicit parameter is easier and communicates better than messing with Maybes and nullables.
foreign import _pushA ::Fn4
                   Foreign
                   (Firebase -> Effect Unit)
                   (FirebaseErr -> Effect Unit)
                   Firebase
                   (Effect Unit)

pushA ::
        Foreign ->
        (Firebase -> Effect Unit) ->
        (FirebaseErr -> Effect Unit) ->
        Firebase ->
        Effect Unit
pushA = runFn4 _pushA

-- Callback receives Maybe on the outside, Nullable on the inside
callBackReceivesNull :: forall a b. (Maybe a -> b) -> (Nullable a -> b)
callBackReceivesNull cb = nab
  where nab nullValue = cb (toMaybe nullValue)

-- | Get the absolute URL for this location -  https://firebase.google.com/docs/reference/js/firebase.database.Reference#toString
foreign import _toString :: Fn1 Firebase (Effect String)

toString :: Firebase -> Effect String
toString ds = runFn1 _toString ds
