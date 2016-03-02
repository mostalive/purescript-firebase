module Test.AuthDataSpec (authDataSpec) where

import Prelude (Unit, show, ($), bind)
import Data.Maybe (Maybe(Just, Nothing))

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Test.Spec (describe, it, Spec())
import Test.Spec.Assertions (shouldEqual)
import Web.Firebase.Authentication.Status (UserCredentials(UserCredentials))
import Web.Firebase.Authentication.Google (GoogleProfile(GoogleProfile))
import Data.Either (Either(..))
import Data.Foreign (ForeignError())
import Data.Foreign.Class (readJSON)

authDataSpec :: forall eff. Spec (eff) Unit
authDataSpec = do
  describe "UserCredentials" do
   it "parses a twitter record" do
      let nullStatus = readJSON twitterLoggedInJson :: Either ForeignError UserCredentials
      case nullStatus of
        Left e -> throwError $ error $ show e
        Right s -> s `shouldEqual` record
   it "parses a google record" do
      let status = readJSON googleLoggedInJson :: Either ForeignError UserCredentials
      status `shouldEqual` (Right googleRecord)

-- approximation. The empty objects have a lot of provider specific data in them
-- e.g. the 'twitter' field has user profile picture etc. different for google etc.
-- so watch out when building a parser for that part.
twitterLoggedInJson :: String
twitterLoggedInJson = """{ "provider": "twitter", "uid": "twitter:16594263", "twitter": {}, "token": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ…", "auth": {}, "expires": 1454431083 }"""

googleLoggedInJson :: String
googleLoggedInJson = """{
"provider": "google",
"uid": "google:114417593001395431343",
"expires": 1487429338,
"google": {
  "displayName": "Willem van den Ende",
  "id": "114417593001395431343",
  "profileImageURL": "https://lh5.googleusercontent.com/-ejZtaRAyRp4/AAAAAAAAAAI/AAAAAAAAAA8/3QAxwh1JjAE/photo.jpg"},
"provider": "google",
"token": "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ2IjowLCJkIjp7InVpZCI6Imdvb2dsZToxMTQ0MTc1OTMwMDEzOTU0MzEzNDMiLCJwcm92aWRlciI6Imdvb2dsZSJ9LCJpYXQiOjE0NTYzMjUzMzh9.LMXWskxCo91pBXmcOgqIpcaYFlAtcpPUi9SI9u31_j0",
"uid": "google:114417593001395431343"} """


record :: UserCredentials
record = UserCredentials {
         provider: "twitter"
         , uid: "twitter:16594263"
         , token: "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ…"
         , google: Nothing
       }

googleRecord :: UserCredentials
googleRecord = UserCredentials {
  provider: "google"
  , uid: "google:114417593001395431343"
  , token: "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ2IjowLCJkIjp7InVpZCI6Imdvb2dsZToxMTQ0MTc1OTMwMDEzOTU0MzEzNDMiLCJwcm92aWRlciI6Imdvb2dsZSJ9LCJpYXQiOjE0NTYzMjUzMzh9.LMXWskxCo91pBXmcOgqIpcaYFlAtcpPUi9SI9u31_j0"
  , google: Just (GoogleProfile {
            displayName: "Willem van den Ende",
            id: "114417593001395431343",
            email: Nothing, -- email is optional depending on permissions the user has granted
            profileImageURL: "https://lh5.googleusercontent.com/-ejZtaRAyRp4/AAAAAAAAAAI/AAAAAAAAAA8/3QAxwh1JjAE/photo.jpg"
          })

}

