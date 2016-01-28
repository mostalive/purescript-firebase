module Web.Firebase
( EventType(..)
, child
, newFirebase
, on
, once
, push
, set
)
where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Foreign (Foreign())
import Data.Function (Fn1(), Fn2(), Fn3(), Fn4(), runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe())
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

foreign import onImpl :: forall eff. Fn4
                   String
                   (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit)
                   (Nullable (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit))
                   Firebase
                   (Eff (firebase :: FirebaseEff | eff) Unit)

on :: forall eff.
      EventType ->
      (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) ->
      Maybe (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit) ->
      Firebase ->
      Eff (firebase :: FirebaseEff | eff) Unit
on etype ds cb fb = runFn4 onImpl (showEventType etype) (unsafeEvalEff <<< ds) (toNullable cb) fb


foreign import onceImpl :: forall eff. Fn4
        String
        (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit)
        (Nullable (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit))
        Firebase
        (Eff (firebase :: FirebaseEff | eff) Unit)

once :: forall eff.
        EventType ->
        (DataSnapshot -> Eff (firebase :: FirebaseEff | eff) Unit) ->
        Maybe (FirebaseErr -> Eff (firebase :: FirebaseEff | eff) Unit) ->
        Firebase ->
        Eff (firebase :: FirebaseEff | eff) Unit
once etype ds cb fb = runFn4 onceImpl (showEventType etype) (unsafeEvalEff <<< ds) (toNullable cb) fb

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
