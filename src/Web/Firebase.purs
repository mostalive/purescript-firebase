module Web.Firebase
( EventType(..)
, child
, newFirebase
, on
, once
, push
, pushE
, set
)
where

import Prelude (Unit(), (<$>), (<<<), ($))
import Control.Monad.Eff (Eff())
import Data.Foreign (Foreign())
import Data.Function (Fn1(), Fn2(), Fn3(), Fn4(), runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable, Nullable())
import Data.URI (printURI)
import Data.URI.Types (URI())
import Web.Firebase.Types (Firebase(), FirebaseEff(), FirebaseErr(), DataSnapshot())
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
on etype ds canceler fb = runFn4 onImpl (showEventType etype) (unsafeEvalEff <<< ds) (unsafeEvalEff <<< canceler) fb

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
foreign import offSimple :: forall eff. Fn1
                Firebase
                (Eff (firebase :: FirebaseEff | eff) Unit)

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
                   (FirebaseErr -> Eff eff Unit)
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Firebase)

pushE :: forall eff.
        Foreign ->
        (FirebaseErr -> Eff eff Unit) ->
        Firebase ->
        Eff (firebase :: FirebaseEff | eff) Firebase
pushE value cb fb = runFn3 pushEImpl value cb fb
