module Control.Safely where

import Prelude

import Control.Monad.Trans (lift)
import Control.Coroutine (runProcess)
import Control.Monad.Rec.Class (MonadRec)

-- | A monad morphism.
type MMorph f g = forall a. f a -> g a

-- | A control operator is an operator which works on any `Monad`,
-- | so that we can change the monad by providing monad morphisms
-- | to wrap and unwrap monadic actions.
class Operator o where
  mapO :: forall m n. MMorph m n -> MMorph n m -> o m -> o n

-- | Make a control operator stack-safe.
safely :: forall o m a. (Operator o, MonadRec m) => (forall t. (Monad t) => o t) -> o m
safely o = mapO runProcess lift o
