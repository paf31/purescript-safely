module Test.Main where

import Prelude

import Data.List
import Data.Functor

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

newtype Mapper m = Mapper (forall a. (a -> m Unit) -> List a -> m Unit)

runMapper :: forall m a. Mapper m -> (a -> m Unit) -> List a -> m Unit
runMapper (Mapper f) = f

instance mapperOperator :: Operator Mapper where
  mapO to fro (Mapper f) = Mapper \p ta -> to (f (fro <<< p) ta) 

traverse_ :: forall m a. (Monad m) => (a -> m Unit) -> List a -> m Unit
traverse_ f = go
  where
  go Nil = return unit
  go (Cons x xs) = do
    f x
    go xs

traverseS :: forall t m a b. (MonadRec m) => (a -> m Unit) -> List a -> m Unit
traverseS = runMapper (safely (Mapper traverse_))

main = do
  traverseS print (Data.List.range 1 1000000)
