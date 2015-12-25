module Web.Firebase.DataSnapshot
( val )
where

import Web.Firebase.Types (DataSnapshot())
import Data.Foreign (Foreign())
import Data.Function (Fn1(), runFn1)

-- | Gets the JavaScript object representation of the DataSnapshot.
foreign import valImpl :: Fn1 DataSnapshot Foreign

val :: DataSnapshot -> Foreign
val = runFn1 valImpl
