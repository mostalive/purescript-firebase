module Web.Firebase.Monad.Aff where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff(), makeAff)
import Control.Monad.Eff.Exception (Error(), error)

import qualified Web.Firebase as FB
import qualified Web.Firebase.Types as FB

on :: forall eff.
      FB.EventType ->
      FB.Firebase ->
      Aff (firebase :: FB.FirebaseEff | eff) FB.DataSnapshot
on etype fb = makeAff (\eb cb -> FB.on etype cb (Just $ onErr eb) fb)
  where
    -- TODO: Find out how to represent the error. Is there something
    -- in foreign for it?
    onErr :: (Error -> Eff (firebase :: FB.FirebaseEff | eff) Unit) ->
             FB.FirebaseErr ->
             Eff (firebase :: Web.Firebase.Types.FirebaseEff | eff) Unit
    onErr eb = const <<< eb $ error "Firebase Error (sorry, I can't tell you what went wrong)"
