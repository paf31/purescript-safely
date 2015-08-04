module Test.Main where

import Prelude

import Data.List

import Control.Safely
import Control.Monad.Rec.Class
import Control.Monad.Eff
import Control.Monad.Eff.Console

newtype Replicator m = Replicator (forall a. Int -> m a -> m (List a))

instance replicator :: Operator Replicator where
  mapO to fro (Replicator r) = Replicator \n m -> to (r n (fro m))

runReplicator :: forall m a. Replicator m -> Int -> m a -> m (List a)
runReplicator (Replicator r) = r

replicateS :: forall m a. (MonadRec m) => Int -> m a -> m (List a)
replicateS = runReplicator (safely (Replicator replicateM))

main = do
  replicateS 100000 (log "Testing...")
  log "If you can see this, everything worked."
