module Web.Firebase
( EventType(..)
, child
, key
, newFirebase
, offSimple
, on
, once
, push
, pushA
, pushE
, set
, setE
, setA
)
where

import Prelude (Unit(), (<$>), (<<<), ($), map)
import Control.Monad.Eff (Eff())
import Data.Foreign (Foreign())
import Data.Function (Fn1(), Fn2(), Fn3(), Fn4(), runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe, toNullable, Nullable())
import Data.URI (printURI)
import Data.URI.Types (URI())
import Web.Firebase.Types (Firebase(), FirebaseEff(), FirebaseErr(), DataSnapshot(), Key())
import Web.Firebase.Unsafe (unsafeEvalEff)


foreign import newFirebaseImpl :: forall eff. Fn1 String (Eff (firebase :: FirebaseEff | eff) Firebase)

newFirebase :: forall eff. URI -> Eff (firebase :: FirebaseEff | eff) Firebase
newFirebase u = runFn1 newFirebaseImpl $ printURI u

-- | Gets a Firebase reference for the location at the specified relative path.
-- https://www.firebase.com/docs/web/api/firebase/child.html
-- Firebase documentation does not specify what happens when the child does not exist
-- Probably nothing happens when the path does not exist, since references don't involve IO, they are just data about paths in the DB that
-- may or may not exist.
-- Existence can be checked by getting the value, and DataSnapshot.exists - https://www.firebase.com/docs/web/api/datasnapshot/exists.html

foreign import childImpl :: forall eff. Fn2 String Firebase (Eff (firebase :: FirebaseEff | eff) Firebase)

child :: forall eff. String -> Firebase -> Eff (firebase :: FirebaseEff | eff) Firebase
child = runFn2 childImpl

-- | Gets the key of the reference - https://www.firebase.com/docs/web/api/firebase/key.html
-- Nothing on the root ref (following the behaviour in the Firebase API)
foreign import _key :: forall eff. Fn1 Firebase (Eff (firebase :: FirebaseEff | eff) (Nullable Key))

key :: forall eff. Firebase -> Eff (firebase :: FirebaseEff | eff) (Maybe Key)
key ds = map toMaybe (runFn1 _key ds)

data EventType = Value
               | ChildAdded
               | ChildChanged
               | ChildRemoved
               | ChildMoved

showEventType :: EventType -> String
showEventType t = case t of
                       Value -> "value"
                       ChildAdded -> "child_added"
                       ChildChanged -> "child_changed"
                       ChildRemoved -> "child_removed"
                       ChildMoved -> "child_moved"

-- | Listens for data changes at a particular location
-- https://www.firebase.com/docs/web/api/query/on.html
-- Error callback in this FFI is not optional, see onWithoutCancelCallbackImpl why.
foreign import onImpl :: forall eff. Fn4
                   String
                   (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit)
                   (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit)
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Unit)

on :: forall eff.
      EventType ->
      (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) ->
      (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit) ->
      Firebase ->
      Eff (firebase :: FirebaseEff | eff) Unit
on etype ds canceler fb = runFn4 onImpl (showEventType etype) ds canceler fb

-- | this function is only present for documentation purposes
-- so you can see why we error callbacks are not optional in this FFI.
--- The documentation mentions the Cancel callback is optional.
-- It does not mention that your program will halt with a Javascript error.
-- When an error occurs while no callback is passed to on,  or when null is passed, it throws a javascript exception:
--  FIREBASE WARNING: Exception was thrown by user callback. TypeError: errorCallback is not a function.
foreign import onWithoutCancelCallbackImpl :: forall eff. Fn3
                   String
                   (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit)
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Unit)

-- | unsubscribes from a location
-- optionally for a specific callback or type. We don't need that in our application right now, so not going to bother with Maybes just now. just take the ref
-- and add the other parameters later.
-- https://www.firebase.com/docs/web/api/query/off.html
foreign import _offSimple :: forall eff. Fn1
                Firebase
                (Eff (firebase :: FirebaseEff | eff) Unit)


offSimple :: forall eff. Firebase -> Eff (firebase :: FirebaseEff | eff) Unit
offSimple ref = runFn1 _offSimple ref

-- implementation of off with more parameters goes here. we can then rewrite offSimple in terms of off.

-- | Listens for one change at a particular location
-- Takes a success and an error callback.
-- Error callback is not optional in this implementation, see onWithoutCancleCallbackImpl for why.
foreign import onceImpl :: forall eff. Fn4
        String
        (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit)
        (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit)
        Firebase
        (Eff (firebase :: FirebaseEff | eff) Unit)

once :: forall eff.
        EventType ->
        (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) ->
        (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit) ->
        Firebase ->
        Eff (firebase :: FirebaseEff | eff) Unit
once etype ds cb fb = runFn4 onceImpl (showEventType etype) (unsafeEvalEff <<< ds) (cb) fb

-- | sets a value in the firebase https://www.firebase.com/docs/web/api/firebase/set.html
-- this implementation is also broken, FirebaseErr can be null. if you implement a callback it will fail
-- fixing setImplE first, purescript semantics make more sense: if you don't want to handle errors,
-- you can allways pass a callback that ignores its input: noop _ = pure Unit
-- Also, push hang when an error would occur and Nothing was passed. Not sure how that happens. This probably behaves similarly.
foreign import setImpl :: forall eff. Fn3
                   Foreign
                   (Nullable (Nullable (FirebaseErr -> Eff eff Unit)))
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Unit)

set :: forall eff.
       Foreign ->
       Maybe (Maybe (FirebaseErr -> Eff eff Unit)) ->
       Firebase ->
       Eff (firebase :: FirebaseEff | eff) Unit
set value cb fb = runFn3 setImpl value (toNullable (toNullable <$> cb)) fb

-- | an extra implementation of set() that forces you to provide an error callback, even if it is a noop (see set in the example above)
-- FirebaseErr can be null, so we wrap it in a Maybe
foreign import setEImpl :: forall eff. Fn3
                   Foreign
                   ((Nullable FirebaseErr) -> Eff eff Unit)
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Unit)

setE :: forall eff.
       Foreign ->
       ((Maybe FirebaseErr) -> Eff eff Unit) ->
       Firebase ->
       Eff (firebase :: FirebaseEff | eff) Unit
setE value cb fb = runFn3 setEImpl value (callBackReceivesNull cb) fb

-- | set with success and error callback, for easy Aff conversion with makeAff
-- explicit parameter is easier and communicates better than messing with Maybes and nullables.
foreign import _setA :: forall eff eff1 eff2. Fn4
                   Foreign
                   (Unit -> Eff eff1 Unit)
                   (FirebaseErr -> Eff eff2 Unit)
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Unit)

setA :: forall eff eff1 eff2.
        Foreign ->
        (Unit -> Eff eff1 Unit) ->
        (FirebaseErr -> Eff eff2 Unit) ->
        Firebase ->
        Eff (firebase :: FirebaseEff | eff) Unit
setA = runFn4 _setA

-- | this one is broken. hangs with Nothing passed for callback (so undefined actually might not work)
-- also hangs when Just (Just callback) is passed, even when calling the effect.
foreign import pushImpl :: forall eff. Fn3
                   Foreign
                   (Nullable (Nullable (FirebaseErr -> Eff eff Unit)))
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Firebase)

push :: forall eff.
        Foreign ->
        Maybe (Maybe (FirebaseErr -> Eff eff Unit)) ->
        Firebase ->
        Eff (firebase :: FirebaseEff | eff) Firebase
push value cb fb = runFn3 pushImpl value (toNullable (toNullable <$> cb)) fb


-- | push with FirebaseErr error callback
-- explicit parameter is easier and communicates better than messing with Maybes and nullables.
foreign import pushEImpl :: forall eff. Fn3
                   Foreign
                   ((Nullable FirebaseErr) -> Eff eff Unit)
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Firebase)

pushE :: forall eff.
        Foreign ->
        ((Maybe FirebaseErr) -> Eff eff Unit) ->
        Firebase ->
        Eff (firebase :: FirebaseEff | eff) Firebase
pushE value cb fb = runFn3 pushEImpl value (callBackReceivesNull cb) fb


-- | push with success and error callback, for easy Aff conversion with makeAff
-- explicit parameter is easier and communicates better than messing with Maybes and nullables.
foreign import _pushA :: forall eff eff1 eff2. Fn4
                   Foreign
                   (Firebase -> Eff eff1 Unit)
                   (FirebaseErr -> Eff eff2 Unit)
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Unit)

pushA :: forall eff eff1 eff2.
        Foreign ->
        (Firebase -> Eff eff1 Unit) ->
        (FirebaseErr -> Eff eff2 Unit) ->
        Firebase ->
        Eff (firebase :: FirebaseEff | eff) Unit
pushA = runFn4 _pushA

-- Callback receives Maybe on the outside, Nullable on the inside
callBackReceivesNull :: forall a b. (Maybe a -> b) -> (Nullable a -> b)
callBackReceivesNull cb = nab
  where nab nullValue = cb (toMaybe nullValue)
