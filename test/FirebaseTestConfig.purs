module FirebaseTestConfig (firebaseConfig) where

-- generated from https://console.firebase.google.com/u/0/project/purescript-spike/overview
import Web.Firebase.Types (FirebaseConfig, mkFirebaseConfig)

firebaseConfig ::  FirebaseConfig
firebaseConfig = mkFirebaseConfig {
    apiKey: "AIzaSyAyPNxC9QM8TsF_EfyyPVcPSpI5W8ARoa8",
    authDomain: "purescript-spike.firebaseapp.com",
    databaseURL: "http://localhost:9000?ns=purescript-spike",
    projectId: "purescript-spike",
    storageBucket: "purescript-spike.appspot.com",
    messagingSenderId: "770384530890"}
-- somewhere else: firebase.initializeApp(config);
