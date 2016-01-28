module PlayWithFire where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Aff (Aff())
import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FBT
import Web.Firebase.Monad.Aff (onceValue)
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

writeWithFire :: forall e. Eff (console :: CONSOLE , firebase :: FBT.FirebaseEff | e) FBT.Firebase
writeWithFire = do
  log "Hello Firebase!"
  let fbUri = fromRight $ runParseURI "https://purescript-spike.firebaseio.com/"
  fb <- FB.newFirebase fbUri
  root <- FB.child "entries" fb
  FB.push (toForeign $ {success: "yes!"}) Nothing root

printSnapshot :: forall e. FBT.DataSnapshot -> Eff (console :: CONSOLE, firebase :: FBT.FirebaseEff | e) Unit
printSnapshot snap = do
  let js =  (FC.readWith foreignErrorToString (val snap)) :: Either String Success
  print js

snapshot2success :: FBT.DataSnapshot -> Either String Success
snapshot2success snap = (FC.readWith foreignErrorToString (val snap)) :: Either String Success

readSuccessAff :: forall eff. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
readSuccessAff ref = do
  snap <- onceValue ref
  let suc = snapshot2success snap
  pure suc

