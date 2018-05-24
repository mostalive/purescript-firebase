module Web.Firebase.AdminSDK
  (
    AdminSDK
  , createCustomToken
  , everythingInJs
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

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (EffFnAff, fromEffFnAff)
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Show (class Show)
import Prelude (($))
import Web.Firebase.Types (FirebaseEff, App)

foreign import data AdminSDK :: Type
foreign import requireAdminSDKImpl :: AdminSDK

adminSDK :: AdminSDK
adminSDK = requireAdminSDKImpl

newtype CredentialCert = CredentialCert String -- JSON
newtype DatabaseName = DatabaseName String
newtype CustomToken = CustomToken String
newtype UserId = UserId String

-- | https://firebase.google.com/docs/admin/setup
-- | AdminSDK is last parameter, so it can be chained without `flip`
foreign import initializeAppImpl :: forall eff. Fn3
   CredentialCert
   DatabaseName
   AdminSDK
   (Eff (firebase :: FirebaseEff | eff) App)

initializeApp :: forall eff.
  CredentialCert ->
  DatabaseName ->
  AdminSDK ->
  (Eff (firebase :: FirebaseEff | eff) App)
initializeApp = runFn3 initializeAppImpl

-- | start without custom claims
-- | https://firebase.google.com/docs/auth/admin/create-custom-tokens#create_custom_tokens_using_the_firebase_admin_sdks
foreign import createCustomTokenImpl :: forall eff.
  UserId ->
  App ->
  EffFnAff (firebase :: FirebaseEff | eff ) CustomToken

createCustomToken :: forall eff.
  UserId ->
  App ->
  (Aff (firebase :: FirebaseEff | eff ) CustomToken)
createCustomToken uid admin =
  fromEffFnAff $ createCustomTokenImpl uid admin

foreign import everythingInJsImpl :: forall eff.
  String ->
  EffFnAff (firebase :: FirebaseEff | eff ) CustomToken

everythingInJs :: forall eff.
  String ->
  Aff (firebase :: FirebaseEff | eff ) CustomToken
everythingInJs jsonFileName =
  fromEffFnAff (everythingInJsImpl jsonFileName)

mkCredentialCert :: String -> CredentialCert
mkCredentialCert = CredentialCert

mkDatabaseName :: String -> DatabaseName
mkDatabaseName = DatabaseName

mkUserId :: String -> UserId
mkUserId = UserId

instance showCustomToken :: Show CustomToken where
  show (CustomToken s) = s
