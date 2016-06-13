module Control.Safely
  ( safely
  , replicateS_
  , traverseS_
  , foldS
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT, runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans (lift)
import Data.Foldable (class Foldable, traverse_)
import Data.Identity (Identity, runIdentity)
import Data.List (List(..))

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
-- | traverseS_ :: forall m a. MonadRec m => (a -> m Unit) -> List a -> m Unit
-- | traverseS_ f xs = safely \lift lower ->
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
safely f = lower (f lift lower)
  where
    lower :: forall f. MonadRec f => FreeT Identity f ~> f
    lower = runFreeT (pure <<< runIdentity)

-- | Safely replicate an action N times.
replicateS_ :: forall m a. MonadRec m => Int -> m a -> m Unit
replicateS_ n x = safely \lift _ ->
  let go i | i <= 0 = pure unit
           | otherwise = do lift x
                            go (i - 1)
  in go n

-- | Safely traverse a foldable container.
traverseS_ :: forall f m a. (Foldable f, MonadRec m) => (a -> m Unit) -> f a -> m Unit
traverseS_ f xs = safely \lift _ -> traverse_ (f >>> lift) xs

-- | Perform a monadic fold, safely.
foldS :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> List b -> m a
foldS step a0 bs = safely \lift _ ->
  let go a Nil = pure a
      go a (Cons x xs) = do
        a' <- lift (step a x)
        go a' xs
  in go a0 bs
