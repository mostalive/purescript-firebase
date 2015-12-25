module Web.Firebase.Unsafe
  ( unsafeEvalEff
  ) where

import Control.Monad.Eff (Eff())

-- | Run an effectful computation maintaining the type signature.
--   This can be helpful when passing callbacks to FFI functions,
--   but comes with obvious big scary risks.
foreign import unsafeEvalEff :: forall eff a. Eff eff a -> Eff eff a
