module Control.Safely 
  ( MMorph()
  , safely
  ) where

import Prelude

import Control.Monad.Trans (lift)
import Control.Coroutine (runProcess)
import Control.Monad.Rec.Class (MonadRec)

-- | A monad morphism.
type MMorph f g = forall a. f a -> g a

-- | Make a monadic action stack-safe.
safely :: forall m a. (MonadRec m) => (forall t. (Monad t) => MMorph m t -> t a) -> m a
safely f = runProcess (f lift)
