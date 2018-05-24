module Web.Firebase.Aff.Database
  (ref
  , rootRefFor
  , goOffline
  , goOnline)
  where

import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Prelude (Unit, ($), (<<<))
import Web.Firebase.Eff.Database as EffDb
import Web.Firebase.Types (Database, FirebaseEff, Reference)

ref :: forall eff1 eff2.
  MonadEff
    ( firebase :: FirebaseEff
    | eff1
    )
    eff2
   => String -> Database -> eff2 Reference
ref aKey db = liftEff $ EffDb.ref aKey db

-- rootRefFor :: forall eff2 eff1.
--   Database ->
--   Aff
--     ( firebase :: FirebaseEff
--     | eff1
--     )
--    Reference
rootRefFor :: forall t5 t7.
  MonadEff
    ( firebase :: FirebaseEff
    | t7
    )
    t5
   => Database -> t5 Reference
rootRefFor = liftEff <<< EffDb.rootRefFor

goOffline :: forall eff2 eff1.
  MonadEff
    ( firebase :: FirebaseEff
    | eff1
    )
    eff2
   => Database -> eff2 Unit
goOffline = liftEff <<< EffDb.goOffline

goOnline :: forall eff2 eff1.
  MonadEff
    ( firebase :: FirebaseEff
    | eff1
    )
    eff2
   => Database -> eff2 Unit
goOnline = liftEff <<< EffDb.goOnline
