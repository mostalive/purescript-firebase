module Web.Firebase.Authentication.Status (
UserCredentials(..)
) where

import Prelude (class Eq, class Show, return, ($), bind)
import Data.Foreign (isNull)
import Data.Foreign.Generic (readGeneric, Options(), defaultOptions)
import Data.Foreign.Class (class IsForeign, read)
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (Maybe)

import Web.Firebase.Authentication.Google (GoogleProfile)

-- | convert authdata (from twitter and google to start with) to a purescript authdata record
-- sample data at https://boiling-heat-7831.firebaseapp.com/authspike.html
-- Maybe we should use an extensible record with an alternative parser, that parses UserCredentials + google/twitter etc
-- because now users have to include all providers in their source, even when they use only one.
newtype UserCredentials = UserCredentials {
    provider :: String
  , uid :: String
  , token :: String
  , google :: Maybe GoogleProfile
}


-- ProviderUserProfile = TwitterProfile TwitterProfileRecord | GoogleProfile GoogleProfileRecord | etc

derive instance genericUserCredentials :: Generic UserCredentials

instance showUserCredentials :: Show UserCredentials where
  show = gShow

instance eqUserCredentials :: Eq UserCredentials where
  eq = gEq

jsonOptions :: Options
jsonOptions = defaultOptions { unwrapNewtypes = true }

-- | keep in mind to check with isNull in the caller if there is an object or not
instance isForeignUserCredentials :: IsForeign UserCredentials where
  read = readGeneric jsonOptions

