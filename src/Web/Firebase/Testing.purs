module Web.Firebase.Testing (
  DatabaseName(..),
  adminApp) where

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Prelude ((<<<))
import Web.Firebase.Types (FirebaseAppImpl)

newtype DatabaseName = DatabaseName String

-- | Wrapper for
-- | https://firebase.google.com/docs/database/security/test-rules-emulator

foreign import _adminApp :: DatabaseName -> EffectFnAff FirebaseAppImpl

adminApp :: DatabaseName -> Aff FirebaseAppImpl
adminApp = fromEffectFnAff <<< _adminApp
