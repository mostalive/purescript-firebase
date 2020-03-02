module Web.Firebase.Unsafe
  ( unsafeEvalEff
  ) where

import Effect (Effect)

-- | Run an effectful computation maintaining the type signature.
--   This can be helpful when passing callbacks to FFI functions,
--   but comes with obvious big scary risks.
foreign import unsafeEvalEff :: forall a. Effect a -> Effect a
-- rename to evalCallbackEff ? it's not unsafe, it is expected.
-- otherwise extra () on JS side - see Aff documentation

-- unsafeEvalNullable :: forall eff a. Maybe (Eff eff a) -> Nullable (Eff eff a)
-- case Nothing -> pure unit
-- case Maybe eff -> unsafeEvalEff eff
