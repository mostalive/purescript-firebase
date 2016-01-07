module Web.Firebase.Types where

foreign import data FirebaseEff :: !

foreign import data Firebase :: *
foreign import data FirebaseErr :: *
foreign import data DataSnapshot :: *

data Key = String
