module Web.Firebase.AdminSDK
  (
    AdminSDK
  , createCustomToken
  , CustomToken
  , CredentialCert
  , mkCredentialCert
  , DatabaseName
  , mkDatabaseName
  , UserId
  , mkUserId
  , adminSDK
  , initializeApp
  ) where

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Web.Firebase.Types (FirebaseEff, App)

foreign import data AdminSDK :: Type -- Object used to initialize AdminSDK and perform operations like authentication on

foreign import requireAdminSDKImpl :: AdminSDK

adminSDK :: AdminSDK
adminSDK = requireAdminSDKImpl

newtype CredentialCert = CredentialCert String -- JSON
newtype DatabaseName = DatabaseName String
newtype CustomToken = CustomToken String
newtype UserId = UserId String

-- | https://firebase.google.com/docs/admin/setup
-- | AdminSDK is last parameter, so it can be chained without `flip`
foreign import initializeAppImpl :: forall eff. Fn3 CredentialCert DatabaseName AdminSDK (Eff (firebase :: FirebaseEff | eff) App)

initializeApp :: forall eff. CredentialCert -> DatabaseName -> AdminSDK -> (Eff (firebase :: FirebaseEff | eff) App)
initializeApp = runFn3 initializeAppImpl

-- | start without custom claims
-- | https://firebase.google.com/docs/auth/admin/create-custom-tokens#create_custom_tokens_using_the_firebase_admin_sdks
foreign import createCustomTokenImpl :: forall eff. Fn2 UserId AdminSDK (Eff (firebase :: FirebaseEff | eff ) CustomToken)

createCustomToken :: forall eff. UserId -> AdminSDK -> (Eff (firebase :: FirebaseEff | eff ) CustomToken)
createCustomToken uid admin = runFn2 createCustomTokenImpl uid admin

mkCredentialCert :: String -> CredentialCert
mkCredentialCert = CredentialCert

mkDatabaseName :: String -> DatabaseName
mkDatabaseName = DatabaseName

mkUserId :: String -> UserId
mkUserId = UserId
