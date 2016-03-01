module Web.Firebase.Authentication.Google (
GoogleProfile(..)
) where

-- | Extra data returned by onAuth on a login with google.
-- Record and foreign generic declarations

import Prelude (class Eq, class Show)
import Data.Maybe (Maybe)
import Data.Foreign.Generic (readGeneric, Options(), defaultOptions)
import Data.Foreign.Class (class IsForeign)
import Data.Generic (class Generic, gEq, gShow)

newtype GoogleProfile = GoogleProfile {
    displayName :: String
  , id :: String
  , email :: Maybe String -- email is optional depending on permissions and account type
  , profileImageURL :: String
}

-- ProviderUserProfile = TwitterProfile TwitterProfileRecord | GoogleProfile GoogleProfileRecord | etc

data AuthenticationStatus = LoggedOut | LoggedIn GoogleProfile -- | LoggedIn GoogleProfile ProviderUserProfile

derive instance genericGoogleProfile :: Generic GoogleProfile

instance showGoogleProfile :: Show GoogleProfile where
  show = gShow

instance eqGoogleProfile :: Eq GoogleProfile where
  eq = gEq

jsonOptions :: Options
jsonOptions = defaultOptions { unwrapNewtypes = true }

instance isForeignGoogleProfile :: IsForeign GoogleProfile where
  read = readGeneric jsonOptions
