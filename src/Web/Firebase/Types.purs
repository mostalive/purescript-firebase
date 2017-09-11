module Web.Firebase.Types (
     DatabaseImpl
   , DataSnapshot
   , Firebase
   , FirebaseConfig
   , FirebaseEff
   , FirebaseErr
   , Key
   , mkFirebaseConfig)
where

-- in process of moving the to string conversion function here, as it belongs with the typeclass
import Control.Monad.Eff (kind Effect)
import Prelude (class Show, class Eq, (==))

foreign import data FirebaseEff :: Effect 

-- backwards compatility, Firebase is now more than a database, but we have some old code to fix
-- and in new code we want to use the stubbable typeclass                    
type Firebase = DatabaseImpl

foreign import data DatabaseImpl :: Type
foreign import data FirebaseErr :: Type

foreign import firebaseErrToString :: FirebaseErr -> String

instance showFirebaseErr :: Show FirebaseErr where
  show err = firebaseErrToString err

instance eqFirebaseErr :: Eq FirebaseErr where
  eq e1 e2 = (firebaseErrToString e1) == (firebaseErrToString e2)

newtype FirebaseConfig = FirebaseConfig FirebaseConfigRecord

mkFirebaseConfig :: FirebaseConfigRecord -> FirebaseConfig
mkFirebaseConfig r = FirebaseConfig r

type FirebaseConfigRecord = {
    apiKey :: String
    , authDomain :: String
    , databaseURL :: String
    , projectId :: String
    , storageBucket :: String
    , messagingSenderId :: String
}

foreign import data FirebaseAppImpl :: Type


{-
  https://www.firebase.com/docs/web/guide/user-auth.html#section-handling-errors:
  All errors are Error objects containing at least code and message attributes. In some cases, additional information will be provided via the details attribute. For example:
  {
	    code: "TRANSPORT_UNAVAILABLE",
       message: "There are no login transports available for the requested method.",
         details: "More details about the specific error here."
 }
-}

--https://www.firebase.com/docs/web/api/datasnapshot/
foreign import data DataSnapshot :: Type

type Key = String
