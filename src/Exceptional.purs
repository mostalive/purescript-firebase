module Exceptional where
import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION(), error, throwException, catchException, message)

--https://github.com/purescript/purescript-exceptions/blob/master/docs/Control/Monad/Eff/Exception.md
--trying to understand purescript book p 105 , handlers and actions

readConfig :: forall eff. Eff (err :: EXCEPTION | eff) Unit
readConfig = throwException exception
  where exception = error "Throwing a Javascript exception"

playWithExceptions = do
  log "exceptions coming up next"
  catchException printException readConfig
  where 
    printException e = do
      log (message e)
