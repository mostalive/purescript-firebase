module PlayWithFFI where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (CONSOLE(),log, print)
import Data.Array (singleton)
import Data.Foreign (F())
import Data.Foreign.Class

foreign import encodeURIComponent :: String -> String

newtype Success = Success { success :: String}

-- https://github.com/purescript/purescript-foreign
-- https://github.com/purescript/purescript-foreign/blob/master/examples/Objects.purs
-- can now be done with generics - http://www.purescript.org/learn/generic/
instance successIsForeign :: IsForeign Success where
 read value = do
   success <- readProp "success" value
   return $ Success { success : success }

instance successShow :: Show Success where
  show (Success s) = "Success { success: " ++ show s.success ++ " }"

-- can't pass [1] or (singleton 1) to it, types do not unify
foreign import unsafeHead :: forall a. Array a -> a

playWithFFI :: forall e. Eff ( console :: CONSOLE | e) Unit
playWithFFI = do
  log $ "encode URI " ++ encodeURIComponent "Hello World"
  log $ "read json object"
  let json = "{\"success\":\"Success\"}"
  {- s <- case v of
        Left err -> show err
        Right su -> su.success
  -}
  print json
  let v = readJSON json :: F Success
  print v
  log $ "unsafe Head"
  x <- unsafeHead []
  log $ x
