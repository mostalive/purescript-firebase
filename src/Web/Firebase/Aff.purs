module Web.Firebase.Aff 
( onceValue
)
where

import Prelude
import Control.Monad.Aff (makeAff, Aff())
import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FBT

-- | This is the start of a more 'purescript-ish' interface than can be found in Web.Firebase
-- We use the Aff monad to eliminate callback hell
-- This way we can deal with callbacks that are called once.
-- We envision Web.Firebase.Signals to generate signals from callbacks that can be called multiple times

-- We also take the liberty to write more specific functions, e.g. once and on() in firebase have 4 event types. We get better error messages and code completion by making specific functions, e.g.
-- onValue and onChildAdded instead of on(Value) and on(ChildAdded) 

once :: forall e. FB.EventType -> FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
once eventType root = makeAff (\error success -> FB.once eventType success root)

onceValue :: forall e. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | e) FBT.DataSnapshot
onceValue root = once FB.Value root
