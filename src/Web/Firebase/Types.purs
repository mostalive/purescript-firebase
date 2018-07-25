module Web.Firebase.Types (
     App
   , DatabaseImpl
   , DataSnapshot
   , Firebase
   , FirebaseAppImpl
   , FirebaseConfig
   -- , FirebaseEff
   , FirebaseErr
   , Key
   , mkFirebaseConfig)
where

-- in process of moving the to string conversion function here, as it belongs with the typeclass
-- import Effect (kind Effect)
import Prelude (class Show, class Eq, (==))

-- foreign import data FirebaseEff :: Effect

-- backwards compatility, Firebase is now more than a database, but we have some old code to fix
-- and in new code we want to use the stubbable typeclass
type Firebase = DatabaseImpl
type App = FirebaseAppImpl

foreign import data FirebaseAppImpl :: Type

foreign import data DatabaseImpl :: Type

foreign import data DataSnapshot :: Type

type Key = String

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
