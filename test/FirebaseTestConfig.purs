module FirebaseTestConfig (firebaseTestDatabase) where

import Control.Monad.Eff (Eff)
import Web.Firebase.Types (Database)
-- on js side: read serviceaccount from testserviceaccount.json, read that into a const,
--

-- | Database test ref, we isolate the config on the js side,
-- | so all tests use the same root ref
foreign import firebaseTestDatabase :: forall eff. Eff eff Database
