module PlayWithFire where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Aff (makeAff, Aff())
import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FB
import Web.Firebase.DataSnapshot (val)
import Data.Foreign
import qualified Data.Foreign.Class as FC
import Data.Either
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

-- maybe use generics for this - http://www.purescript.org/learn/generic/
instance successEq :: Eq Success where
  eq (Success s1) (Success s2) = s1.success == s2.success

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
  FB.push (toForeign $ {success: "yes!"}) Nothing root
  log "Write success!" 

printSnapshot :: forall e. FB.DataSnapshot -> Eff (console :: CONSOLE, firebase :: FB.FirebaseEff | e) Unit
printSnapshot snap = do
  let js =  (FC.readWith foreignErrorToString (val snap)) :: Either String Success
  print js

readWithFire :: forall e. Eff (console :: CONSOLE , firebase :: FB.FirebaseEff | e) Unit
readWithFire = do
  log "implementation for reading from firebase goes here"
  let fbUri = fromRight $ runParseURI "https://purescript-spike.firebaseio.com/"
  fb <- FB.newFirebase fbUri
  root <- FB.child "entries" fb
  FB.once FB.ChildAdded printSnapshot root
  log "Read passed." 

readSuccess :: forall e. Eff (console :: CONSOLE , firebase :: FB.FirebaseEff | e) Unit
readSuccess = do
  log "implementation for reading from firebase goes here"
  let fbUri = fromRight $ runParseURI "https://purescript-spike.firebaseio.com/"
  fb <- FB.newFirebase fbUri
  root <- FB.child "entries" fb
  FB.once FB.ChildAdded printSnapshot root
  log "Read passed." 

snapshot2success :: FB.DataSnapshot -> Either String Success
snapshot2success snap = (FC.readWith foreignErrorToString (val snap)) :: Either String Success

readSuccessAff :: forall eff. FB.Firebase -> Aff (firebase :: FB.FirebaseEff | eff) (Either String Success)
readSuccessAff root = do
  snap <- onceAff root
  let suc = snapshot2success snap
  pure suc


onceAff :: forall e. FB.Firebase -> Aff (firebase :: FB.FirebaseEff | e) FB.DataSnapshot
onceAff root = makeAff (\error success -> FB.once FB.ChildAdded success root)
