module FirebaseTestConfig (firebaseConfig) where

-- generated from https://console.firebase.google.com/u/0/project/purescript-spike/overview

firebaseConfig ::  { apiKey :: String
    , authDomain :: String
    , databaseURL :: String
    , projectId :: String
    , storageBucket :: String
    , messagingSenderId :: String
    }
firebaseConfig = {
    apiKey: "AIzaSyAyPNxC9QM8TsF_EfyyPVcPSpI5W8ARoa8",
    authDomain: "purescript-spike.firebaseapp.com",
    databaseURL: "https://purescript-spike.firebaseio.com",
    projectId: "purescript-spike",
    storageBucket: "purescript-spike.appspot.com",
    messagingSenderId: "770384530890"
}
-- somewhere else: firebase.initializeApp(config);
