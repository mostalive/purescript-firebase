module Web.Firebase.Testing (
  DatabaseName(..),
  UserId,
  initializeAdminApp) where

import Control.Promise (Promise, toAff) as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Prelude (Unit, ($), (<<<), (>>=))
import Web.Firebase.Types (FirebaseAppImpl)

newtype DatabaseName = DatabaseName String
newtype UserId = UserId String  -- | e.g. UserId "alice"

-- | Wrapper for
-- | https://firebase.google.com/docs/database/security/test-rules-emulator .
-- | Fiunctions have the same name as in @firebase/testing.
-- | Interfaces are minimal:
-- | they match the examples, and the purescript types are narrow
-- | so it is easy to use them safely, without additional conversions
-- | in test code.

foreign import _adminApp :: DatabaseName -> EffectFnAff FirebaseAppImpl

initializeAdminApp :: DatabaseName -> Aff FirebaseAppImpl
initializeAdminApp = fromEffectFnAff <<< _adminApp

type AuthRecord = {
  databaseName:: String,
  auth:: {uid:: String}
}

foreign import _initializeTestApp :: AuthRecord -> EffectFnAff FirebaseAppImpl

initializeTestApp :: DatabaseName -> UserId -> Aff FirebaseAppImpl
initializeTestApp (DatabaseName dbName) (UserId uid) = fromEffectFnAff $
  _initializeTestApp {
  databaseName: dbName,
  auth: {uid}
}

foreign import _initializeAnonymousTestApp :: DatabaseName -> EffectFnAff FirebaseAppImpl
-- | Firebase examples pass 'null' for a UID. We don't mimic that,
-- | in order to keep callers clean.
initializeAnonymousTestApp :: DatabaseName -> Aff FirebaseAppImpl
initializeAnonymousTestApp = fromEffectFnAff <<< _initializeAnonymousTestApp

type RulesFor = {
  databaseName :: DatabaseName,
  rules :: String
}

foreign import _loadDatabaseRules :: RulesFor -> Effect (Promise.Promise Unit)

loadDatabaseRules :: RulesFor -> Aff Unit
loadDatabaseRules rules = liftEffect (_loadDatabaseRules rules) >>=  Promise.toAff
