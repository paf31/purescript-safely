module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Control.Safely (foldM, replicateM_, traverse_)
import Data.List (range)

main :: Effect Unit
main = void do
  replicateM_ 10000 (log "Testing...")
  traverse_ logShow (range 1 10000)
  foldM (\a b -> logShow a *> pure (a + b)) 0 (range 1 10000)
