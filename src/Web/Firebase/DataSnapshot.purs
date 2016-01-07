module Web.Firebase.DataSnapshot
(
exists,	
val, 
hasChild,
numChildren)
where

import Web.Firebase.Types (DataSnapshot())
import Data.Foreign (Foreign())
import Data.Function (Fn1(), runFn1, Fn2(), runFn2)

-- | Returns true if this DataSnapshot contains any data.
-- https://www.firebase.com/docs/web/api/datasnapshot/exists.html
foreign import _exists :: Fn1 DataSnapshot Boolean

exists :: DataSnapshot -> Boolean
exists = runFn1 _exists

-- | Gets the JavaScript object representation of the DataSnapshot.
foreign import valImpl :: Fn1 DataSnapshot Foreign

val :: DataSnapshot -> Foreign
val = runFn1 valImpl


-- | Returns true if the specified child exists
-- https://www.firebase.com/docs/web/api/datasnapshot/haschild.html

foreign import _hasChild :: Fn2 DataSnapshot String Boolean

hasChild :: DataSnapshot -> String -> Boolean
hasChild = runFn2 _hasChild

-- | Returns the number of children
-- https://www.firebase.com/docs/web/api/datasnapshot/numchildren.html
foreign import _numChildren :: Fn1 DataSnapshot Int

numChildren :: DataSnapshot -> Int
numChildren = runFn1 _numChildren

