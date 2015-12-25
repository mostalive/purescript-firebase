module PlayWithFire where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FB
import Data.Foreign
import qualified Data.Foreign.Class as FC
import Data.Either (Either(Left, Right))
import Data.Either.Unsafe  (fromRight)
import Data.Maybe (Maybe(..))
import Data.URI (runParseURI)

newtype Success = Success { success :: String}

{- https://github.com/purescript/purescript-foreign
 https://github.com/purescript/purescript-foreign/blob/master/examples/Objects.purs
 can now be done with generics - http://www.purescript.org/learn/generic/
-}

instance successIsForeign :: FC.IsForeign Success where
 read value = do
   success <- FC.readProp "success" value
   return $ Success { success : success }

instance successShow :: Show Success where
  show (Success s) = "Success { success: " ++ show s.success ++ " }"

foreignErrorToString :: ForeignError -> String
foreignErrorToString f = show f

aSuccessHandler :: Foreign -> Eff (console :: CONSOLE) Unit
aSuccessHandler frown = do
  let js = (FC.readWith foreignErrorToString frown) :: Either String Success
  print js

writeWithFire :: forall e. Eff (console :: CONSOLE , firebase :: FB.FirebaseEff | e) Unit
writeWithFire = do
  log "Hello Firebase!"
  let fbUri = fromRight $ runParseURI "https://purescript-spike.firebaseio.com/"
  fb <- FB.newFirebase fbUri
  root <- FB.child "entries" fb
  FB.push (toForeign $ {status: "success"}) Nothing root
  log "Write success!" 

readWithFire :: forall e. Eff (console :: CONSOLE , firebase :: FB.FirebaseEff | e) Unit
readWithFire = do
  log "implementation for reading from firebase goes here"
{-
  -- start without error handling. Might call it unsafe or withoutErrorChecking
  FB.onceChildAdded root aSuccessHandler
  -- Ds.val to disappear into handler. It should return a Foreign
  frown <- DS.val ds :: Foreign
-}
  -- Figure out how to exit application after FB.push. 
  -- maybe purescript is waiting for some scripts to be wrapped up.
  -- we probably need to turn this into an Async
