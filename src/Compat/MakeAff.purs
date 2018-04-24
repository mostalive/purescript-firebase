module Compat.MakeAff (makeAff') where

-- | old makeAff behaviour on the new Aff, so that we can update our library in small steps
-- | rewriting everything to 'new' Aff in one go wasn't much fun. And I'm using Aff less and less, and not using cancelers so far in client code.
-- | thanks to @natefaubion for suggesting it in #purescript on functionalprogramming.slack.com
import Control.Monad.Aff (Aff, Error, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Data.Either (Either(..))
import Prelude (Unit, ($>), (<<<))

-- | old makeAff behaviour on the new Aff, so that we can update our library in small steps
-- | rewriting everything to 'new' Aff in one go wasn't much fun. And I'm using Aff less and less, and not using cancelers so far in client code.
-- | thanks to @natefaubion for suggesting it in #purescript on functionalprogramming.slack.com
makeAff' :: forall t2 t3 t5. ((Error -> Eff t3 Unit) -> (t2 -> Eff t3 Unit) -> Eff t3 t5) -> Aff t3 t2
makeAff' oldAff = makeAff \k -> oldAff (k <<< Left) (k <<< Right) $> nonCanceler
