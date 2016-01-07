module Web.Firebase.DataSnapshot
(
exists,	
val, 
hasChild,
hasChildren,
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
-- val can be null if the snapshot is empty or does not exist
foreign import valImpl :: Fn1 DataSnapshot Foreign

val :: DataSnapshot -> Foreign
val = runFn1 valImpl


-- | Returns true if the specified child exists
-- https://www.firebase.com/docs/web/api/datasnapshot/haschild.html

foreign import _hasChild :: Fn2 DataSnapshot String Boolean

hasChild :: DataSnapshot -> String -> Boolean
hasChild = runFn2 _hasChild

-- | Returns true if the DataSnapshot has children
-- https://www.firebase.com/docs/web/api/datasnapshot/haschildren.html
foreign import _hasChildren :: Fn1 DataSnapshot Boolean

hasChildren :: DataSnapshot -> Boolean
hasChildren = runFn1 _hasChildren

-- | Returns the number of children
-- https://www.firebase.com/docs/web/api/datasnapshot/numchildren.html
foreign import _numChildren :: Fn1 DataSnapshot Int

numChildren :: DataSnapshot -> Int
numChildren = runFn1 _numChildren

