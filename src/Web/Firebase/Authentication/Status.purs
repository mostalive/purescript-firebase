module Web.Firebase.Authentication.Status (
UserCredentials(..),
AuthenticationStatus(..)
) where

import Prelude (class Eq, class Show, return, ($), bind)
import Data.Foreign (isNull)
import Data.Foreign.Generic (readGeneric, Options(), defaultOptions)
import Data.Foreign.Class (class IsForeign, read)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe)

import Web.Firebase.Authentication.Google (GoogleProfile)

-- convert authdata (from twitter to start with) to a purescript authdata record
-- sample data at https://boiling-heat-7831.firebaseapp.com/authspike.html
-- Maybe we should use an extensible record with an alternative parser, that parses UserCredentials + google/twitter etc
-- because now users have to include all providers in their source, even when they use only one.
newtype UserCredentials = UserCredentials {
    provider :: String
  , uid :: String
  , token :: String
 -- , google :: Maybe GoogleProfile
}

-- ProviderUserProfile = TwitterProfile TwitterProfileRecord | GoogleProfile GoogleProfileRecord | etc

data AuthenticationStatus = UnAuthenticated | Authenticated UserCredentials -- | LoggedIn UserCredentials ProviderUserProfile

derive instance genericUserCredentials :: Generic UserCredentials

instance showUserCredentials :: Show UserCredentials where
  show = gShow

instance eqUserCredentials :: Eq UserCredentials where
  eq = gEq

jsonOptions :: Options
jsonOptions = defaultOptions { unwrapNewtypes = true }

instance isForeignUserCredentials :: IsForeign UserCredentials where
  read = readGeneric jsonOptions

derive instance authenticationStatusIsGeneric :: Generic AuthenticationStatus

instance authenticationShow :: Show AuthenticationStatus where
  show = gShow

instance authenticationeq :: Eq AuthenticationStatus where
  eq = gEq

instance authenticationStatusIsForeign :: IsForeign AuthenticationStatus where
  read value | isNull value  = return UnAuthenticated
  read value = do
    credentials <- readGeneric jsonOptions value
    return $ Authenticated credentials
