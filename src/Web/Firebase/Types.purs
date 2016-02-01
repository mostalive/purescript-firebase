module Web.Firebase.Types where

foreign import data FirebaseEff :: !

foreign import data Firebase :: *
foreign import data FirebaseErr :: *

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
foreign import data DataSnapshot :: *

type Key = String
