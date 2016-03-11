module Test.Authentication (authenticationSpec) where

import Prelude (Unit, bind, ($), map, show)

import Control.Monad.Aff (launchAff, attempt)
import Control.Monad.Aff.AVar (AVAR(), makeVar, takeVar, putVar)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION(), message)
import Data.Either (either)
import Data.Maybe (Maybe(Just))
import Data.Foreign (toForeign)
import Web.Firebase.Types as FBT
import Web.Firebase (EventType(..),once, pushE, push)
import Test.Spec                  (describe, it, Spec())
import Test.Spec.Assertions       (shouldEqual)
import Test.Spec.Assertions.Aff (expectError)
import Web.Firebase.Monad.Aff (onceValue, on, firebaseErrToString)
import Web.Firebase.Authentication.Eff (authWithCustomToken)

authenticationSpec :: forall eff. FBT.Firebase -> Spec (firebase :: FBT.FirebaseEff, err :: EXCEPTION, avar :: AVAR, console :: CONSOLE | eff ) Unit
authenticationSpec forbiddenRef = do
    describe "Authentication" do
      describe "with Eff" do
        it "on fake authentication token calls an error callback" do
          respVar <- makeVar
          handle  <- liftEff $ authWithCustomToken
                                 "imaginarytoken"
                                 (\_   -> launchAff $ putVar respVar "unexpected sucess")
                                 (\err -> launchAff $ putVar respVar (show err))
                                 forbiddenRef
          actual <- takeVar respVar
          actual `shouldEqual` "INVALID_TOKEN: Could not parse auth token.\n | firebase code: | \n INVALID_TOKEN"

