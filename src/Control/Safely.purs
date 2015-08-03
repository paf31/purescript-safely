module Control.Safely 
  ( safely
  ) where

import Prelude

import Control.Monad.Trans (lift)
import Control.Coroutine (runProcess)
import Control.Monad.Rec.Class (MonadRec)

safely :: forall m a. (MonadRec m) => (forall t. (Monad t) => (forall a. m a -> t a) -> (forall a. t a -> m a) -> m a) -> m a
safely f = f lift runProcess
