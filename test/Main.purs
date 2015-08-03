module Test.Main where

import Prelude

import Data.List

import Control.Safely
import Control.Monad.Rec.Class
import Control.Monad.Eff
import Control.Monad.Eff.Console

replicateS :: forall m a. (MonadRec m) => Int -> m a -> m (List a)
replicateS n m = safely \up -> replicateM n (up m)

main = do
  replicateS 100000 (log "Testing...")
  log "If you can see this, everything worked."
