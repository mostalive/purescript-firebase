module Web.Firebase.Aff.Database where

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff)
import Prelude (($))
import Web.Firebase.Eff.Database (ref) as EffDb
import Web.Firebase.Types as FBT

ref :: forall eff.
       FBT.Key ->
       FBT.Database ->
       Aff (firebase :: FBT.FirebaseEff | eff) FBT.Reference
ref aKey db = liftEff $ EffDb.ref aKey db
