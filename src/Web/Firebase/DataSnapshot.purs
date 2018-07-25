module Web.Firebase.DataSnapshot
(
exists,
key,
hasChild,
hasChildren,
numChildren,
child,
val)
where

import Web.Firebase.Types (DataSnapshot(), Key())
import Foreign (Foreign()) -- does not work with 0.7.6
import Data.Nullable (toMaybe, Nullable())
import Data.Maybe (Maybe)
import Data.Function.Uncurried (Fn1(), runFn1, Fn2(), runFn2)

-- | Returns true if this DataSnapshot contains any data.
-- https://www.firebase.com/docs/web/api/datasnapshot/exists.html
foreign import _exists :: Fn1 DataSnapshot Boolean

exists :: DataSnapshot -> Boolean
exists = runFn1 _exists

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
-- You'll only find out if it exists if you call functions on it. See e.g. val and hasChild
-- https://www.firebase.com/docs/web/api/datasnapshot/numchildren.html
foreign import _numChildren :: Fn1 DataSnapshot Int

numChildren :: DataSnapshot -> Int
numChildren = runFn1 _numChildren

-- | Returns a child snapshot of the current snapshot
-- https://www.firebase.com/docs/web/api/datasnapshot/child.html
foreign import _child :: Fn2 DataSnapshot String DataSnapshot

child :: DataSnapshot -> String -> DataSnapshot
child = runFn2 _child


-- | Gets the JavaScript object representation of the DataSnapshot.
-- val can be null if the snapshot is empty or does not exist
foreign import valImpl :: Fn1 DataSnapshot Foreign

val :: DataSnapshot -> Foreign
val = runFn1 valImpl

-- | Gets the key of the location that generated the DataSnapshot
foreign import _key :: Fn1 DataSnapshot (Nullable Key)

key :: DataSnapshot -> Maybe Key
key ds = toMaybe (runFn1 _key ds)
