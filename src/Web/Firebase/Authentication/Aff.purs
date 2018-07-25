module Web.Firebase.Authentication.Aff (authWithCustomToken) where

import Data.Either (Either(..))
import Control.Monad ((>>=))
import Effect.Aff (Aff, makeAff)
import Foreign (Foreign)
import Prelude (unit, (<<<), const, pure, ($))
import Web.Firebase.Aff (convertError, noOpCanceler)
import Web.Firebase.Authentication.Eff as AE
import Web.Firebase.Authentication.Types (Auth)
import Web.Firebase.Types as FBT

authWithCustomToken :: String ->
                       Auth ->
                       Aff Foreign
authWithCustomToken token ref = makeAff (\fn -> AE.authWithCustomToken token (fn <<< Right) (convertError (fn <<< Left)) ref >>= const (pure noOpCanceler) )
