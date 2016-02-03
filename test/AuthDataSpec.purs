module Test.AuthDataSpec (authDataSpec) where

import Prelude (Unit, show, ($), bind)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (describe, it, Spec())
import Test.Spec.Assertions (shouldEqual)
import Web.Firebase.Authentication.Status (AuthenticationStatus(LoggedIn, LoggedOut), UserCredentials(UserCredentials))
import Data.Either (Either(..))
import Data.Foreign (ForeignError())
import Data.Foreign.Class (readJSON)

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

