module Web.Firebase.Eff.Database
 ( goOffline,
   goOnline,
   ref,
   rootRefFor
 ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (EffFn1, runEffFn1, EffFn2, runEffFn2)
import Prelude (Unit)
import Web.Firebase.Types (Database, App, Reference, FirebaseEff)

foreign import app :: Database -> App
-- | Database goes second, so calls can be easily chained
foreign import refImpl :: forall eff. EffFn2 (firebase :: FirebaseEff | eff) String Database Reference

ref :: forall eff. String -> Database -> Eff (firebase :: FirebaseEff | eff) Reference
ref = runEffFn2 refImpl

-- | equivalent to ref() without parameters
foreign import rootRefForImpl :: forall eff. EffFn1 (firebase :: FirebaseEff | eff) Database Reference
rootRefFor :: forall eff. Database -> Eff (firebase :: FirebaseEff | eff) Reference
rootRefFor = runEffFn1 rootRefForImpl

foreign import goOfflineImpl :: forall eff. EffFn1 (firebase :: FirebaseEff | eff) Database Unit
goOffline :: forall eff. Database -> Eff (firebase :: FirebaseEff | eff) Unit
goOffline = runEffFn1 goOfflineImpl

foreign import goOnlineImpl :: forall eff. EffFn1 (firebase :: FirebaseEff | eff) Database Unit
goOnline :: forall eff. Database -> Eff (firebase :: FirebaseEff | eff) Unit
goOnline = runEffFn1 goOnlineImpl
