module Control.Safely
  ( safely
  , replicateM_
  , traverse_
  , for_
  , foldM
  ) where

import Prelude
import Data.Foldable as Foldable
import Control.Monad.Free.Trans (FreeT, runFreeT)
import Control.Monad.Rec.Class (tailRecM2, Step(..), tailRecM, class MonadRec)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (class Foldable)
import Data.Identity (Identity)
import Data.List (List(..), (:))
import Data.Newtype (unwrap)

-- | Make a control operator stack-safe.
-- |
-- | For any `MonadRec`, we can form the `Monad` `FreeT Identity m` in which
-- | arbitrarily-associated binds are stack-safe.
-- |
-- | This function performs the wrapping and unwrapping for us, and provides a
-- | pair of natural transformations to lift and lower actions to and from the
-- | safe monad.
-- |
-- | This means that we can write stack-safe monadic code for any `MonadRec` by
-- | simply writing the naive implementation, and wrapping it in a call to
-- | `safely`:
-- |
-- | ```purescript
-- | traverseSafely :: forall m a. MonadRec m => (a -> m Unit) -> List a -> m Unit
-- | traverseSafely f xs = safely \lift lower ->
-- |   let
-- |     go Nil = pure unit
-- |     go (Cons x xs) = do
-- |       lift (f x)
-- |       go xs
-- |   in go xs
-- | ```
safely
  :: forall m a
   . MonadRec m
  => (forall safe. MonadRec safe => (m ~> safe) -> (safe ~> m) -> safe a)
  -> m a
safely f = lower (f lift lower) where
  lower :: forall f. MonadRec f => FreeT Identity f ~> f
  lower = runFreeT (pure <<< unwrap)

-- | Safely replicate an action N times.
replicateM_ :: forall m a. MonadRec m => Int -> m a -> m Unit
replicateM_ n x = tailRecM step n where
  step :: Int -> m (Step Int Unit)
  step 0 = pure (Done unit)
  step m = x $> Loop (m - 1)

-- | Safely traverse a foldable container.
traverse_ :: forall f m a. Foldable f => MonadRec m => (a -> m Unit) -> f a -> m Unit
traverse_ f xs = safely \lift _ -> Foldable.traverse_ (f >>> lift) xs

-- | Safely traverse a foldable container.
for_ :: forall f m a. Foldable f => MonadRec m => f a -> (a -> m Unit) -> m Unit
for_ = flip traverse_

-- | Perform a monadic fold, safely.
foldM :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> List b -> m a
foldM f = tailRecM2 step where
  step :: a -> List b -> m (Step { a :: a, b :: List b } a)
  step a Nil = pure (Done a)
  step a (b : bs) = do
    a' <- f a b
    pure (Loop { a: a', b: bs })
