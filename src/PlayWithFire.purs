module PlayWithFire where

import Prelude (class Eq, class Show, Unit(), pure, bind, ($), show, (==), (<>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Aff (Aff())
import Web.Firebase as FB
import Web.Firebase.Types as FBT
import Web.Firebase.Monad.Aff (onceValue)
import Web.Firebase.DataSnapshot (val)
import Web.Firebase.UnsafeRef (unsafeRef)
import Data.Foreign (Foreign(), ForeignError(), toForeign)
import Data.Foreign.Class as FC
import Data.Either (Either())
import Data.Maybe (Maybe(..))

newtype Success = Success { success :: String}

{- https://github.com/purescript/purescript-foreign
 https://github.com/purescript/purescript-foreign/blob/master/examples/Objects.purs
 can now be done with generics - http://www.purescript.org/learn/generic/
 but generics have their own gotchas for now.
-}

instance successIsForeign :: FC.IsForeign Success where
 read value = do
   success <- FC.readProp "success" value
   pure $ Success { success : success }

instance successShow :: Show Success where
  show (Success s) = "Success { success: " <> show s.success <> " }"

-- maybe use generics for this - http://www.purescript.org/learn/generic/
instance successEq :: Eq Success where
  eq (Success s1) (Success s2) = s1.success == s2.success

foreignErrorToString :: ForeignError -> String
foreignErrorToString f = show f

snapshot2success :: FBT.DataSnapshot -> Either String Success
snapshot2success snap = (FC.readWith foreignErrorToString (val snap)) :: Either String Success

readSuccessAff :: forall eff. FBT.Firebase -> Aff (firebase :: FBT.FirebaseEff | eff) (Either String Success)
readSuccessAff ref = do
  snap <- onceValue ref
  let suc = snapshot2success snap
  pure suc

