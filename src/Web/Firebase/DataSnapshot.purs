module Web.Firebase.DataSnapshot
( val, 
hasChild)
where

import Web.Firebase.Types (DataSnapshot())
import Data.Foreign (Foreign())
import Data.Function (Fn1(), runFn1, Fn2(), runFn2)

-- | Gets the JavaScript object representation of the DataSnapshot.
foreign import valImpl :: Fn1 DataSnapshot Foreign

val :: DataSnapshot -> Foreign
val = runFn1 valImpl


-- | Returns true if the specified child exists
-- https://www.firebase.com/docs/web/api/datasnapshot/haschild.html

foreign import _hasChild :: Fn2 DataSnapshot String Boolean

hasChild :: DataSnapshot -> String -> Boolean
hasChild = runFn2 _hasChild
