module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import PlayWithFire as F
import Exceptional as E

import Web.Firebase.Types as FB

main :: forall e. Eff ( console :: CONSOLE, firebase :: FB.FirebaseEff | e) Unit
main = do
  E.playWithExceptions
  F.readWithFire
