module Web.Firebase.Authentication.Aff (authWithCustomToken) where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Foreign (Foreign)
import Web.Firebase.Aff (convertError)
import Web.Firebase.Authentication.Eff as AE
import Web.Firebase.Authentication.Types (Auth)

authWithCustomToken ::
                       String ->
                       Auth ->
                       Aff Foreign
authWithCustomToken token ref = makeAff (\cb  -> AE.authWithCustomToken token (Right >>> cb) (convertError (Left >>> cb)) ref *> pure nonCanceler)
