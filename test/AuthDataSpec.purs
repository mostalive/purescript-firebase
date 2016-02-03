module Test.AuthDataSpec (authDataSpec) where

import Prelude
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (describe, it, pending, Spec())
import Test.Spec.Assertions (shouldEqual)
import Data.Foreign
import Data.Foreign.Generic
import Data.Foreign.Class
import Data.Either
import Data.Maybe
import Data.Generic


-- convert authdata (from twitter to start with) to a purescript authdata record
-- sample data at https://boiling-heat-7831.firebaseapp.com/authspike.html
newtype UserCredentials = UserCredentials {
          provider :: String
          , uid :: String
          , token :: String
          , expires :: Int
          }

-- ProviderUserProfile = TwitterProfile TwitterProfileRecord | GoogleProfile GoogleProfileRecord | etc

data AuthenticationStatus = LoggedOut | LoggedIn UserCredentials -- | LoggedIn UserCredentials ProviderUserProfile

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
  read value | isNull value  = return LoggedOut
  read value = do
    credentials <- read value
    return $ LoggedIn credentials -- (UserCredentials {provider, uid, token, auth, expires })



authDataSpec :: forall eff. Spec (eff) Unit
authDataSpec = do

  describe "Authentication status" do
   it "parses null to LoggedOut" do
      let nullStatus = readJSON "null" :: Either ForeignError AuthenticationStatus
      case nullStatus of
        Left e -> throwError $ error $ show e
        Right s -> s `shouldEqual` LoggedOut
   it "parses a record to LoggedIn" do
      let nullStatus = readJSON twitterLoggedInJson :: Either ForeignError AuthenticationStatus
      case nullStatus of
        Left e -> throwError $ error $ show e
        Right s -> s `shouldEqual` (LoggedIn record)

-- approximation. The empty objects have a lot of provider specific data in them
-- e.g. the 'twitter' field has user profile picture etc. different for google etc.
-- so watch out when building a parser for that part.
twitterLoggedInJson :: String
twitterLoggedInJson = """{ "provider": "twitter", "uid": "twitter:16594263", "twitter": {}, "token": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ…", "auth": {}, "expires": 1454431083 }"""

record :: UserCredentials
record = UserCredentials { provider: "twitter", uid: "twitter:16594263", token: "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ…", expires: 1454431083 }

-- twitter has: displayName: "Willem van den Ende, id: 165 etc, username: mostalive, profileImageUrl
-- and accessToken and accessTokenSecret. not sure if we need those.
-- auth has : {provider: "twitter", uid : "twitter:16etc"}

