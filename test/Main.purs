module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Safely (foldS, replicateS_, traverseS_)
import Data.List (range)

main :: Eff (console :: CONSOLE) Unit
main = do
  replicateS_ 10000 (log "Testing...")
  traverseS_ logShow (range 1 10000)
  void (foldS (\a b -> logShow a *> pure (a + b)) 0 (range 1 10000))
