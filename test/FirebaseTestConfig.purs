module FirebaseTestConfig (firebaseTestRef) where

import Control.Monad.Eff (Eff)
import Web.Firebase.Types (Firebase)
-- on js side: read serviceaccount from testserviceaccount.json, read that into a const,
--

-- | Firebase test ref, we isolate the config on the js side,
-- | so all tests use the same root ref
foreign import firebaseTestRef :: forall eff. Eff eff Firebase
