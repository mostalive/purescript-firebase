module Web.Firebase.Aff.Read
(
  onceValue
, readRecord
, readOnceWithDefault
, readSnapshot
, readSnapshotWithDefault
, valueAt
) where
import Prelude (pure, show, bind, ($), (<<<))

import Data.Foreign (Foreign)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)

import Data.Either (Either(Left,Right))

import Data.Foreign.Class (class IsForeign, readWith)

import Web.Firebase as FB
import Web.Firebase.Types as FBT
import Web.Firebase.DataSnapshot (exists, val)
import Web.Firebase.Aff (once)

-- | Inspired by its Eff relative.
-- | Throw takes a message and throws a MonadError in Aff with that message
-- TODO move to its own module, also used by 'regular' Aff module
throw :: forall eff a. String -> Aff eff a
throw = throwError <<< error

valueAt :: forall eff. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) Foreign
valueAt ref = do
       snap <- onceValue ref
       pure $ (val snap)

onceValue :: forall e. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
onceValue root = once FB.Value root


-- | read a record from a reference. Throws exception when any part fails
-- possible failures include network, database and conversion from Foreign value to purescript.
readRecord :: forall a eff. (IsForeign a) => FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) a
readRecord ref = do
  value <- valueAt ref
  case (readWith show value) of
    Left msg  -> throw msg
    Right v   -> pure v

-- | Read a record or newtype from a reference. Returns default when no data present at location.
-- an exception will be thrown for other failures, such as network, database
-- and failed conversion from Foreign value to purescript.
readOnceWithDefault :: forall a eff. (IsForeign a) => a -> FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) a
readOnceWithDefault default ref = do
  snap <- onceValue ref
  if (exists snap) then (readSnapshot snap) else (pure default)

-- | read a snapshot to a record. Throws exception when any part fails
-- possible failures include network, database and conversion from Foreign value to purescript.
readSnapshot :: forall a eff. (IsForeign a) => FBT.DataSnapshot -> Aff (firebase :: FBT.FirebaseEff | eff) a
readSnapshot snapshot = do
  let value = val snapshot
  case (readWith show value) of
    Left msg  -> throw msg
    Right v   -> pure v

-- | read a snapshot to a record. Throws exception when any part fails
-- possible failures include network, database and conversion from Foreign value to purescript.
readSnapshotWithDefault :: forall a eff. (IsForeign a) => a -> FBT.DataSnapshot -> Aff (firebase :: FBT.FirebaseEff | eff) a
readSnapshotWithDefault default snapshot = do
  if (exists snapshot) then (readSnapshot snapshot) else (pure default)


